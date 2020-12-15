#!/usr/bin/env node

// This file is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// For a full copy of the GNU General Public License
// see <http://www.gnu.org/licenses/>.

var VERSION = 1;
var SCHEMA = {"type": "object",
              "properties": {
                  "id": { "type": "number" },
                  "data": { "type": "string" },
                  "inline": { "type": "boolean" }
              },
              required: ["id", "data", "inline"],
              additionalProperties: false
             };

var validate = require('jsonschema');
var mjAPI = require("mathjax-node");
var readline = require('readline');

var rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

mjAPI.config({displayErrors: false,
              extensions: "TeX/autoload-all.js"});
mjAPI.start();

// FIXME: catch stderr output
rl.on('line',
      (line) => {
          var output = {"id": 0, "data": null, "error": null};
          var input = {};
          try {
              input = JSON.parse(line);
              validate.validate(input, SCHEMA, {throwFirst: true});
              output.id = input.id;

              mjAPI.typeset({
                  math: input.data,
                  format: input.inline ? "inline-TeX": "TeX",
                  svg:true,
              }, function (data) {
                  if (!data.errors) {
                      output.data = data.svg;
                  } else {
                      output.error = data.errors;
                  }
                  console.log(JSON.stringify(output));
              });
          } catch(E) {
              if (E instanceof SyntaxError) {
                  output.error = "JSON parse error";
              } else if (E instanceof validate.ValidatorResultError) {
                  output.error = "Schema mismatch";
              } else {
                  output.error = "Unknown error";
              }
              console.log(JSON.stringify(output));
          }
      });
