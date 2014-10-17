// doc.js --- Documentation generator for Node.js

// Copyright (C) 2013 Maokai Lin

// Version: 0.1
// Keywords: Node.js Auto-complete Documentation Emacs
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
    util = require("util"),
    common = require("./common");

var errorHead = "!!ERROR!!";

function getDocumentOf(expr, context) {
  try {
    var obj = vm.runInContext(expr, context), doc = "";
    
    if (typeof obj === 'function') {
      var bits = expr.split('.');
      var funName = bits.pop();
      var funStr = obj.toString();
      doc = "Function "+ expr + funStr.slice(funStr.indexOf("("), funStr.indexOf(')') + 1) + "\n\n" + obj.toString();
    } else {
      var type = (typeof obj);
      type = type.charAt(0).toUpperCase() + type.slice(1) // Capitalize
      doc = type + " " + expr + "\n\n  " + util.inspect(obj) + "  ";
    }
    if (doc.length > 300) {
      doc = doc.slice(0, 296) + " ...";
    }
  } catch (e) {
    return e.toString();
  }
  return doc;
}

try {
  var contextScripts = process.argv[2],
      documentFor = process.argv[3];
      
  var context = common.createContext(),
      scriptList = contextScripts.split("~~>");
  for (var scriptK in scriptList) {
    // Run line by line and skip errors
    try {
      vm.runInContext(common.filterScripts(scriptList[scriptK]), context);
    } catch (e) {}
  }
  
  console.log(getDocumentOf(documentFor, context));
  
} catch (e) {
  console.log(errorHead);
  console.log(e.stack);
}
