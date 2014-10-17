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

// This file provides some common functions and variables used in both ac.js and
// docgen.js

var vm = require("vm"),
    express = require("express");

var builtinLibs = ['assert', 'buffer', 'child_process', 'cluster',
                   'crypto', 'dgram', 'dns', 'domain', 'events', 'fs', 'http', 'https', 'net',
                   'os', 'path', 'punycode', 'querystring', 'readline', 'stream',
                   'string_decoder', 'tls', 'tty', 'url', 'util', 'vm', 'zlib'];

function filterScripts(scripts) {
  // Filter the context scripts
  // More filters to come...
  return scripts
    .replace(/.*listen.*/g, '');
    // .replace(/something else/g, '');
}

// Create a new V8 context
function createContext() {
  var context = vm.createContext();
  
  for (var i in global) context[i] = global[i];
  context.global = context;
  context.global.global = context;

  // Copy all the globals
  context.process = process;
  context.console = console;
  context.Buffer = Buffer;
  context.require = require;
  context.__filename = __filename;
  context.__dirname = __dirname;
  context.module = module;
  context.exports = exports;
  context.setTimeout = setTimeout;
  context.clearTimeout = clearTimeout;
  context.setInterval = setInterval;
  context.clearInterval = clearInterval;
  
  // For Express, further default var names setup can be done here
  context.express = express;
  context.req = context.request = express.request;
  context.res = context.response = express.response;
  context.err = new Error("Error class");
  context.app = new express();

  // Make built-in modules available directly (loaded lazily)
  builtinLibs.forEach(function(name) {
    Object.defineProperty(context, name, {
      get: function() {
        var lib = require(name);
        context._ = context[name] = lib;
        return lib;
      },
      // allow the creation of other globals with this name
      set: function(val) {
        delete context[name];
        context[name] = val;
      },
      configurable: true
    });
  });

  return context;
};

// Export key functions
exports.builtinLibs = builtinLibs;
exports.createContext = createContext;
exports.filterScripts = filterScripts;
