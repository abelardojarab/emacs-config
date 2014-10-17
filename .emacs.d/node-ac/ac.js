// ac.js --- Auto-complete candidate generator for Node.js

// Copyright (C) 2013 Maokai Lin

// Version: 0.1
// Keywords: Node.js Auto-complete Emacs
// Author: Maokai Lin <Maokai.Lin@gmail.com>
// URL: https://github.com/MaokaiLin/node-ac

// This file is NOT part of GNU Emacs.

// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later version.

// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.

// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 675 Mass
// Ave, Cambridge, MA 02139, USA.

// The auto-complete functions are borrowed and simplified from the Node.js REPL
// code. Refer to the original code and document:
//   https://github.com/joyent/node/blob/master/lib/repl.js

// Usage:
//   Call function `autocomplte` to get the auto-complete candidates. The
//   function returns an array of candidate strings.

// Example:
//   autocomplete "Math.s"
// returns: [ 'sin', 'sqrt' ]

var vm = require("vm"),
    path = require("path"),
    fs = require("fs"),
    util = require("util"),
    express = require("express");

var common = require("./common");

var errorHead = "!!ERROR!!";

// hack for repl require to work properly with node_modules folders
module.paths = require('module')._nodeModulePaths(module.filename);

var globalProperties = ['NaN', 'Infinity', 'undefined',
                        'eval', 'parseInt', 'parseFloat', 'isNaN', 'isFinite', 'decodeURI',
                        'decodeURIComponent', 'encodeURI', 'encodeURIComponent',
                        'Object', 'Function', 'Array', 'String', 'Boolean', 'Number',
                        'Date', 'RegExp', 'Error', 'EvalError', 'RangeError',
                        'ReferenceError', 'SyntaxError', 'TypeError', 'URIError',
                        'Math', 'JSON'];

var commonKeywords = ['break', 'case', 'catch', 'const',
                      'continue', 'debugger', 'default', 'delete', 'do', 'else',
                      'export', 'false', 'finally', 'for', 'function', 'if',
                      'import', 'in', 'instanceof', 'let', 'new', 'null', 'return',
                      'switch', 'this', 'throw', 'true', 'try', 'typeof', 'undefined',
                      'var', 'void', 'while', 'with', 'yield'];

var shouldAddStandardGlobals = false;

var requireRE = /\brequire\s*\(['"](([\w\.\/-]+\/)?([\w\.\/-]*))/;
var simpleExpressionRE = /(([a-zA-Z_$](?:\w|\$|\[|\]|\(|\))*)\.)*([a-zA-Z_$](?:\w|\$|\[|\]|\(|\))*)\.?$/;

function autocomplete(line, context) {
  var completions;

  // list of completion lists, one for each inheritance "level"
  var completionGroups = [];

  var completeOn, match, filter, i, j, group, c;

  if (match = line.match(requireRE)) {
    // require('...<Tab>')
    var exts = Object.keys(require.extensions);
    var indexRe = new RegExp('^index(' + exts.map(regexpEscape).join('|') + ')$');

    completeOn = match[1];
    filter = match[1];
    var subdir = match[2] || '';
    var dir, files, f, name, base, ext, abs, subfiles, s;
    group = [];
    var paths = module.paths.concat(require('module').globalPaths);
    for (i = 0; i < paths.length; i++) {
      dir = path.resolve(paths[i], subdir);
      try {
        files = fs.readdirSync(dir);
      } catch (e) {
        continue;
      }
      for (f = 0; f < files.length; f++) {
        name = files[f];
        ext = path.extname(name);
        base = name.slice(0, -ext.length);
        if (base.match(/-\d+\.\d+(\.\d+)?/) || name === '.npm') {
          // Exclude versioned names that 'npm' installs.
          continue;
        }
        if (exts.indexOf(ext) !== -1) {
          if (!subdir || base !== 'index') {
            group.push(subdir + base);
          }
        } else {
          abs = path.resolve(dir, name);
          try {
            if (fs.statSync(abs).isDirectory()) {
              group.push(subdir + name + '/');
              subfiles = fs.readdirSync(abs);
              for (s = 0; s < subfiles.length; s++) {
                if (indexRe.test(subfiles[s])) {
                  group.push(subdir + name);
                }
              }
            }
          } catch (e) {}
        }
      }
    }
    if (group.length) {
      completionGroups.push(group);
    }

    if (!subdir) {
      completionGroups.push(common.builtinLibs);
    }

    return completionGroupsLoaded();

    // Handle variable member lookup.
    // We support simple chained expressions like the following (no function
    // calls, etc.). That is for simplicity and also because we *eval* that
    // leading expression so for safety (see WARNING above) don't want to
    // eval function calls.
    //
    // foo.bar<|> # completions for 'foo' with filter 'bar'
    // spam.eggs.<|> # completions for 'spam.eggs' with filter ''
    // foo<|> # all scope vars with filter 'foo'
    // foo.<|> # completions for 'foo' with filter ''
  } else if (line[line.length - 1].match(/\w|\.|\$/)) {
    if (match = simpleExpressionRE.exec(line)) {
      var expr;
      completeOn = (match ? match[0] : '');
      if (line[line.length - 1] === '.') {
        filter = '';
        expr = match[0].slice(0, match[0].length - 1);
      } else {
        // Extract expr and filter.
        // Example: foo.bar.b<TAB>, set expr = "foo.bar", filter = "b"
        var bits = match[0].split('.');
        filter = bits.pop();
        expr = bits.join('.');
      }

      // Resolve expr and get its completions.
      var obj, memberGroups = [];
      if (!expr) {
        // If completion is at the first level ("fooba", not "foo.ba"), push all
        // global symbols into candidate pool
        var contextProto = context;
        while (contextProto = Object.getPrototypeOf(contextProto)) {
          completionGroups.push(Object.getOwnPropertyNames(contextProto));
        }
        completionGroups.push(Object.getOwnPropertyNames(context));
        shouldAddStandardGlobals = true;
        return completionGroupsLoaded();
      } else {
        // If completion is not at the first level ("foo.ba", not "fooba"), push
        // all properties and methods into the candidate pool
        try {
          obj = vm.runInContext(expr, context);
        } catch (e) {
          console.log(errorHead);
          console.log(e);
          return [];
        }
        
        if (obj != null) {
          if (typeof obj === 'object' || typeof obj === 'function') {
            memberGroups.push(Object.getOwnPropertyNames(obj));
          }
          // works for non-objects
          try {
            var sentinel = 5;
            var p;
            if (typeof obj === 'object' || typeof obj === 'function') {
              p = Object.getPrototypeOf(obj);
            } else {
              p = obj.constructor ? obj.constructor.prototype : null;
            }
            while (p !== null) {
              memberGroups.push(Object.getOwnPropertyNames(p));
              p = Object.getPrototypeOf(p);
              // Circular refs possible? Let's guard against that.
              sentinel--;
              if (sentinel <= 0) {
                break;
              }
            }
          } catch (e) {
            console.log(errorHead);
            console.log("Completion error walking prototype chain:\n" + e);
          }
        }
        
        if (memberGroups.length) {
          var filterStartWith_ = (filter.indexOf("_") == 0);
          for (i = 0; i < memberGroups.length; i++) {
            completionGroups.push(memberGroups[i].filter(function(member) {
              return filterStartWith_ || member.indexOf("_") != 0;  // Exclude the ones starting with "_" (normally only for internal use)
            }));
          }
        }
        
        return completionGroupsLoaded();
      }
    } else {
      return completionGroupsLoaded();
    }
  } else {
    return completionGroupsLoaded();
  }

  // Will be called when all completionGroups are in place
  // Useful for async autocompletion
  function completionGroupsLoaded(err) {
    if (err) throw err;

    // Filter, sort (within each group), uniq and merge the completion groups.
    if (completionGroups.length && filter) {
      var newCompletionGroups = [];
      for (i = 0; i < completionGroups.length; i++) {
        group = completionGroups[i].filter(function(elem) {
          return elem.indexOf(filter) == 0;
        });
        if (group.length) {
          newCompletionGroups.push(group);
        }
      }
      completionGroups = newCompletionGroups;
    }

    if (completionGroups.length) {
      var uniq = {}; // unique completions across all groups
      completions = [];
      // Completion group 0 is the "closest"
      // (least far up the inheritance chain)
      // so we put its completions last: to be closest in the REPL.
      for (i = completionGroups.length - 1; i >= 0; i--) {
        group = completionGroups[i];
        group.sort();
        for (var j = 0, doc; j < group.length; j++) {
          c = group[j];
          if (!hasOwnProperty(c)) {
            completions.push(formatOutput(expr, c, context));
            uniq[c] = true;
          }
        }
      }
      while (completions.length && completions[completions.length - 1] === '') {
        completions.pop();
      }
    }

    if (shouldAddStandardGlobals) {
      addStandardGlobals(completions, filter);
    }

    // return [completions || [], completeOn];
    return completions || [];
  }

  function formatOutput(expr, prop, context) {
    var fullExpression = (expr == "" ? prop : expr + "." + prop),
        doc = "No available document.",
        discription = "undefined",
        symbolName = "?",
        snippet = "";  // For yasnippet
    
    try {
      var obj = vm.runInContext(fullExpression, context);
      var type = typeof obj;
      symbolName = type.slice(0, 1);
      discription = type;
      if (typeof obj === 'function') {
        doc = obj.toString();
        var params = doc.slice(doc.indexOf("(") + 1, doc.indexOf(')')).split(",")
              .map(function (elem) { return elem.trim(); });
        // console.log(params);
        var paramSnippets = params.map(function (elem, n) {
          return elem === "" ? "" : util.format("${%d:%s}", n + 1, elem);
        });
        // console.log(paramSnippets);
        
        snippet = prop + "(" + paramSnippets.join(", ") + ")$0";
        prop = prop + "(" + params.join(", ") + ")";  // In terms of functionName(param1, param2, ...)
      } else {
        doc = util.inspect(obj);
      }
      // Take the first 10 lines if too many
      var docLines = doc.split("\n");
      if (docLines.length > 10) {
        doc = docLines.slice(0, 10).join("\n") + " ...";
      }
      if (doc.length > 320) {
        doc = doc.slice(0, 316) + " ...";
      }
    } catch (e) {
      // return e.toString();
    }
    return prop + ">~<" + symbolName + ">~<" + discription + ">~<" + doc + ">~<" + snippet;
  }
}

// If obj.hasOwnProperty has been overridden, then calling
// obj.hasOwnProperty(prop) will break.
// See: https://github.com/joyent/node/issues/1707
function hasOwnProperty(obj, prop) {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

function addStandardGlobals(completions, filter) {
  // Global object properties
  globalProperties.forEach(function (elem) {
    if ((!filter) || elem.indexOf(filter) == 0) {
      completions.push(elem + ">~<g>~<global>~<Global property>~<");
    }
  });
  // Common keywords.
  if (filter) {
    commonKeywords.map(function (elem) {
      if (elem.indexOf(filter) == 0) {
        completions.push(elem + ">~<k>~<keyword>~<Keyword>~<");
      }
    });
  }
}

function regexpEscape(s) {
  return s.replace(/[-\[\]{}()*+?.,\\^$|#\s]/g, '\\$&');
}

try {
  var contextScripts = process.argv[2],
      toBeCompleted = process.argv[3];

  var context = common.createContext(),
      scriptList = contextScripts.split("~~>");
  for (var scriptK in scriptList) {
    // Run line by line and skip errors
    try {
      vm.runInContext(common.filterScripts(scriptList[scriptK]), context);
    } catch (e) {}
  }
  
  var candidates = autocomplete(toBeCompleted, context);
  console.log(candidates.join("~~<"));
} catch (e) {
  console.log(errorHead);
  console.log(e.stack);
}
