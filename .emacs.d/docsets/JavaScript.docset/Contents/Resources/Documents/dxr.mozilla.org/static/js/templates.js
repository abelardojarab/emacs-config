(function() {(window.nunjucksPrecompiled = window.nunjucksPrecompiled || {})["partial/results_container.html"] = (function() {function root(env, context, frame, runtime, cb) {
var lineno = null;
var colno = null;
var output = "";
try {
env.getTemplate("partial/results.html", function(t_2,t_1) {
if(t_2) { cb(t_2); return; }
t_1.getExported(function(t_3,t_1) {
if(t_3) { cb(t_3); return; }
if(t_1.hasOwnProperty("results_list")) {
var t_4 = t_1.results_list;
} else {
cb(new Error("cannot import 'results_list'")); return;
}
context.setVariable("results_list", t_4);
env.getTemplate("partial/results_header.html", function(t_6,t_5) {
if(t_6) { cb(t_6); return; }
t_5.getExported(function(t_7,t_5) {
if(t_7) { cb(t_7); return; }
if(t_5.hasOwnProperty("results_header")) {
var t_8 = t_5.results_header;
} else {
cb(new Error("cannot import 'results_header'")); return;
}
context.setVariable("results_header", t_8);
output += runtime.suppressValue((lineno = 3, colno = 15, runtime.callWrap(t_8, "results_header", [runtime.contextOrFrameLookup(context, frame, "result_count"),runtime.contextOrFrameLookup(context, frame, "result_count_formatted"),runtime.contextOrFrameLookup(context, frame, "top_of_tree"),runtime.contextOrFrameLookup(context, frame, "tree_tuples"),runtime.contextOrFrameLookup(context, frame, "tree")])), env.autoesc);
output += "\n\n";
if(env.getFilter("length").call(context, runtime.contextOrFrameLookup(context, frame, "results")) > 0) {
output += "\n  <table class=\"results\">\n    <caption class=\"visually-hidden\">Query matches</caption>\n    <thead class=\"visually-hidden\">\n      <th scope=\"col\">Line</th>\n      <th scope=\"col\">Code Snippet</th>\n    </thead>\n    ";
output += runtime.suppressValue((lineno = 12, colno = 17, runtime.callWrap(t_4, "results_list", [runtime.contextOrFrameLookup(context, frame, "results"),runtime.contextOrFrameLookup(context, frame, "www_root"),runtime.contextOrFrameLookup(context, frame, "tree")])), env.autoesc);
output += "\n  </table>\n";
;
}
output += "\n";
cb(null, output);
})})})});
} catch (e) {
  cb(runtime.handleError(e, lineno, colno));
}
}
return {
root: root
};
})();
})();
(function() {(window.nunjucksPrecompiled = window.nunjucksPrecompiled || {})["partial/results.html"] = (function() {function root(env, context, frame, runtime, cb) {
var lineno = null;
var colno = null;
var output = "";
try {
env.getTemplate("partial/result_lines.html", function(t_2,t_1) {
if(t_2) { cb(t_2); return; }
t_1.getExported(function(t_3,t_1) {
if(t_3) { cb(t_3); return; }
if(t_1.hasOwnProperty("result_lines")) {
var t_4 = t_1.result_lines;
} else {
cb(new Error("cannot import 'result_lines'")); return;
}
context.setVariable("result_lines", t_4);
var macro_t_5 = runtime.makeMacro(
["results", "www_root", "tree"], 
[], 
function (l_results, l_www_root, l_tree, kwargs) {
frame = frame.push();
kwargs = kwargs || {};
frame.set("results", l_results);
frame.set("www_root", l_www_root);
frame.set("tree", l_tree);
var output= "";
frame = frame.push();
var t_8 = l_results;
if(t_8) {for(var t_6=0; t_6 < t_8.length; t_6++) {
var t_9 = t_8[t_6];
frame.set("result", t_9);
output += "\n    <tbody class=\"result\" data-path=\"";
output += runtime.suppressValue(runtime.memberLookup((t_9),"path", env.autoesc), env.autoesc);
output += "\">\n      <tr class=\"result-head ";
if(runtime.memberLookup((t_9),"is_binary", env.autoesc)) {
output += " binary_row ";
;
}
output += "\">\n        <td class=\"left-column\">\n          <div class=\"";
output += runtime.suppressValue(runtime.memberLookup((t_9),"iconClass", env.autoesc), env.autoesc);
output += " icon-container\"></div>\n        </td>\n        <td>\n          ";
output += runtime.suppressValue(env.getFilter("safe").call(context, runtime.memberLookup((t_9),"pathLine", env.autoesc)), env.autoesc);
output += "\n        </td>\n      </tr>\n      ";
output += runtime.suppressValue((lineno = 13, colno = 19, runtime.callWrap(t_4, "result_lines", [t_9,l_www_root,l_tree])), env.autoesc);
output += "\n    </tbody>\n  ";
;
}
}
frame = frame.pop();
frame = frame.pop();
return new runtime.SafeString(output);
});
context.addExport("results_list");
context.setVariable("results_list", macro_t_5);
output += "\n\n";
output += "\n";
output += runtime.suppressValue((lineno = 19, colno = 13, runtime.callWrap(macro_t_5, "results_list", [runtime.contextOrFrameLookup(context, frame, "results"),runtime.contextOrFrameLookup(context, frame, "www_root"),runtime.contextOrFrameLookup(context, frame, "tree")])), env.autoesc);
output += "\n";
cb(null, output);
})});
} catch (e) {
  cb(runtime.handleError(e, lineno, colno));
}
}
return {
root: root
};
})();
})();
(function() {(window.nunjucksPrecompiled = window.nunjucksPrecompiled || {})["partial/result_lines.html"] = (function() {function root(env, context, frame, runtime, cb) {
var lineno = null;
var colno = null;
var output = "";
try {
var macro_t_1 = runtime.makeMacro(
["result", "www_root", "tree"], 
[], 
function (l_result, l_www_root, l_tree, kwargs) {
frame = frame.push();
kwargs = kwargs || {};
frame.set("result", l_result);
frame.set("www_root", l_www_root);
frame.set("tree", l_tree);
var output= "";
frame = frame.push();
var t_4 = runtime.memberLookup((l_result),"lines", env.autoesc);
if(t_4) {for(var t_2=0; t_2 < t_4.length; t_2++) {
var t_5 = t_4[t_2];
frame.set("entry", t_5);
output += "\n    <tr>\n      <td class=\"left-column\">\n        <a href=\"";
output += runtime.suppressValue(l_www_root, env.autoesc);
output += "/";
output += runtime.suppressValue(l_tree, env.autoesc);
output += "/source/";
output += runtime.suppressValue(runtime.memberLookup((l_result),"path", env.autoesc), env.autoesc);
output += "#";
output += runtime.suppressValue(runtime.memberLookup((t_5),"line_number", env.autoesc), env.autoesc);
output += "\">\n          ";
output += runtime.suppressValue(runtime.memberLookup((t_5),"line_number", env.autoesc), env.autoesc);
output += "\n        </a>\n      </td>\n      <td>\n        <a href=\"";
output += runtime.suppressValue(l_www_root, env.autoesc);
output += "/";
output += runtime.suppressValue(l_tree, env.autoesc);
output += "/source/";
output += runtime.suppressValue(runtime.memberLookup((l_result),"path", env.autoesc), env.autoesc);
output += "#";
output += runtime.suppressValue(runtime.memberLookup((t_5),"line_number", env.autoesc), env.autoesc);
output += "\">\n          <code aria-labelledby=\"";
output += runtime.suppressValue(runtime.memberLookup((t_5),"line_number", env.autoesc), env.autoesc);
output += "\">";
output += runtime.suppressValue(env.getFilter("safe").call(context, runtime.memberLookup((t_5),"line", env.autoesc)), env.autoesc);
output += "</code>\n        </a>\n      </td>\n    </tr>\n  ";
;
}
}
frame = frame.pop();
frame = frame.pop();
return new runtime.SafeString(output);
});
context.addExport("result_lines");
context.setVariable("result_lines", macro_t_1);
output += "\n\n";
output += "\n";
output += runtime.suppressValue((lineno = 18, colno = 13, runtime.callWrap(macro_t_1, "result_lines", [runtime.contextOrFrameLookup(context, frame, "result"),runtime.contextOrFrameLookup(context, frame, "www_root"),runtime.contextOrFrameLookup(context, frame, "tree")])), env.autoesc);
output += "\n";
cb(null, output);
;
} catch (e) {
  cb(runtime.handleError(e, lineno, colno));
}
}
return {
root: root
};
})();
})();
(function() {(window.nunjucksPrecompiled = window.nunjucksPrecompiled || {})["partial/switch_tree.html"] = (function() {function root(env, context, frame, runtime, cb) {
var lineno = null;
var colno = null;
var output = "";
try {
var macro_t_1 = runtime.makeMacro(
["tree_tuples", "selected_tree"], 
[], 
function (l_tree_tuples, l_selected_tree, kwargs) {
frame = frame.push();
kwargs = kwargs || {};
frame.set("tree_tuples", l_tree_tuples);
frame.set("selected_tree", l_selected_tree);
var output= "";
output += "\n  ";
if(env.getFilter("length").call(context, l_tree_tuples) > 1) {
output += "\n    <section id=\"tree-selector\" class=\"tree-selector\">\n      <button type=\"button\" class=\"ts-select-trigger\" aria-label=\"Switch Tree\">\n        <!-- arrow icon using icon font -->\n        <span aria-hidden=\"true\" data-icon-arrow=\"&#xe801;\" class=\"selector-arrow\">\n          <!-- tree icon using icon font -->\n          <span aria-hidden=\"true\" data-icon=\"&#xe800;\"></span>\n          <span class='current-tree'>Switch Tree</span>\n        </span>\n      </button>\n      <div class=\"select-options ts-modal\" aria-expanded=\"false\">\n        <form name=\"options-filter\" class=\"options-filter\" data-active=\"false\">\n          <label for=\"filter-txt\" class=\"visually-hidden\">Filter Trees</label>\n          <input type=\"text\" name=\"filter-txt\" id=\"filter-txt\" placeholder=\"Filter trees\" />\n          <input type=\"submit\" value=\"Filter\" class=\"visually-hidden\" />\n        </form>\n        <ul class=\"selector-options\" tabindex=\"-1\">\n          ";
frame = frame.push();
var t_4 = l_tree_tuples;
if(t_4) {var t_2;
if(runtime.isArray(t_4)) {
for(t_2=0; t_2 < t_4.length; t_2++) {
var t_5 = t_4[t_2][0]
frame.set("tree", t_4[t_2][0]);
var t_6 = t_4[t_2][1]
frame.set("url", t_4[t_2][1]);
var t_7 = t_4[t_2][2]
frame.set("description", t_4[t_2][2]);
output += "\n            <li>\n              <a href=\"";
output += runtime.suppressValue(t_6, env.autoesc);
output += "\" ";
if(t_5 == l_selected_tree) {
output += "class=\"selected\" aria-checked=\"true\"";
;
}
output += ">\n                <span class=\"selector-option-label\">";
output += runtime.suppressValue(t_5, env.autoesc);
output += "</span>\n                <span class=\"selector-option-description\">";
output += runtime.suppressValue(t_7, env.autoesc);
output += "</span>\n              </a>\n            </li>\n          ";
;
}
} else {
t_2 = -1;
for(var t_8 in t_4) {
t_2++;
var t_9 = t_4[t_8];
frame.set("tree", t_8);
frame.set("url", t_9);
output += "\n            <li>\n              <a href=\"";
output += runtime.suppressValue(t_9, env.autoesc);
output += "\" ";
if(t_8 == l_selected_tree) {
output += "class=\"selected\" aria-checked=\"true\"";
;
}
output += ">\n                <span class=\"selector-option-label\">";
output += runtime.suppressValue(t_8, env.autoesc);
output += "</span>\n                <span class=\"selector-option-description\">";
output += runtime.suppressValue(t_7, env.autoesc);
output += "</span>\n              </a>\n            </li>\n          ";
;
}
}
}
frame = frame.pop();
output += "\n        </ul>\n      </div>\n    </section>\n  ";
;
}
output += "\n";
frame = frame.pop();
return new runtime.SafeString(output);
});
context.addExport("tree_menu");
context.setVariable("tree_menu", macro_t_1);
output += "\n";
cb(null, output);
;
} catch (e) {
  cb(runtime.handleError(e, lineno, colno));
}
}
return {
root: root
};
})();
})();
(function() {(window.nunjucksPrecompiled = window.nunjucksPrecompiled || {})["partial/results_header.html"] = (function() {function root(env, context, frame, runtime, cb) {
var lineno = null;
var colno = null;
var output = "";
try {
env.getTemplate("partial/switch_tree.html", function(t_2,t_1) {
if(t_2) { cb(t_2); return; }
t_1.getExported(function(t_3,t_1) {
if(t_3) { cb(t_3); return; }
if(t_1.hasOwnProperty("tree_menu")) {
var t_4 = t_1.tree_menu;
} else {
cb(new Error("cannot import 'tree_menu'")); return;
}
context.setVariable("tree_menu", t_4);
var macro_t_5 = runtime.makeMacro(
["result_count", "result_count_formatted", "top_of_tree", "tree_tuples", "tree"], 
[], 
function (l_result_count, l_result_count_formatted, l_top_of_tree, l_tree_tuples, l_tree, kwargs) {
frame = frame.push();
kwargs = kwargs || {};
frame.set("result_count", l_result_count);
frame.set("result_count_formatted", l_result_count_formatted);
frame.set("top_of_tree", l_top_of_tree);
frame.set("tree_tuples", l_tree_tuples);
frame.set("tree", l_tree);
var output= "";
output += "<p class=\"top-of-tree\">\n    ";
output += runtime.suppressValue(l_result_count_formatted, env.autoesc);
output += " ";
output += runtime.suppressValue((l_result_count == 1?"result":"results"), env.autoesc);
output += " from the <a href=\"";
output += runtime.suppressValue(l_top_of_tree, env.autoesc);
output += "\">";
output += runtime.suppressValue(l_tree, env.autoesc);
output += "</a> tree";
if(l_result_count > 0) {
output += ":";
;
}
output += "\n  </p>\n\n  ";
output += runtime.suppressValue((lineno = 7, colno = 12, runtime.callWrap(t_4, "tree_menu", [l_tree_tuples,l_tree])), env.autoesc);
frame = frame.pop();
return new runtime.SafeString(output);
});
context.addExport("results_header");
context.setVariable("results_header", macro_t_5);
cb(null, output);
})});
} catch (e) {
  cb(runtime.handleError(e, lineno, colno));
}
}
return {
root: root
};
})();
})();
(function() {(window.nunjucksPrecompiled = window.nunjucksPrecompiled || {})["context_menu.html"] = (function() {function root(env, context, frame, runtime, cb) {
var lineno = null;
var colno = null;
var output = "";
try {
output += "<ul id=\"context-menu\" class=\"context-menu\" tabindex=\"0\">\n    ";
frame = frame.push();
var t_3 = runtime.contextOrFrameLookup(context, frame, "menuItems");
if(t_3) {for(var t_1=0; t_1 < t_3.length; t_1++) {
var t_4 = t_3[t_1];
frame.set("item", t_4);
output += "\n        <li><a href=\"";
output += runtime.suppressValue(runtime.memberLookup((t_4),"href", env.autoesc), env.autoesc);
output += "\" class=\"";
output += runtime.suppressValue(runtime.memberLookup((t_4),"icon", env.autoesc), env.autoesc);
output += " icon\">";
output += runtime.suppressValue(env.getFilter("safe").call(context, env.getFilter("default").call(context, runtime.memberLookup((t_4),"html", env.autoesc),runtime.memberLookup((t_4),"text", env.autoesc))), env.autoesc);
output += "</a></li>\n    ";
;
}
}
frame = frame.pop();
output += "\n</ul>\n";
cb(null, output);
;
} catch (e) {
  cb(runtime.handleError(e, lineno, colno));
}
}
return {
root: root
};
})();
})();
(function() {(window.nunjucksPrecompiled = window.nunjucksPrecompiled || {})["path_line.html"] = (function() {function root(env, context, frame, runtime, cb) {
var lineno = null;
var colno = null;
var output = "";
try {
if(!runtime.contextOrFrameLookup(context, frame, "is_first_or_only")) {
output += "<span class=\"path-separator\">/</span>";
;
}
if(!runtime.contextOrFrameLookup(context, frame, "is_binary") || runtime.contextOrFrameLookup(context, frame, "is_dir")) {
output += "<a href=\"";
output += runtime.suppressValue(runtime.contextOrFrameLookup(context, frame, "url"), env.autoesc);
output += "\"";
if(runtime.contextOrFrameLookup(context, frame, "is_dir")) {
output += " data-path=\"";
output += runtime.suppressValue(runtime.contextOrFrameLookup(context, frame, "data_path"), env.autoesc);
output += "\" ";
;
}
output += ">";
;
}
output += runtime.suppressValue(runtime.contextOrFrameLookup(context, frame, "display_path"), env.autoesc);
if(!runtime.contextOrFrameLookup(context, frame, "is_binary") || runtime.contextOrFrameLookup(context, frame, "is_dir")) {
output += "</a>";
;
}
cb(null, output);
;
} catch (e) {
  cb(runtime.handleError(e, lineno, colno));
}
}
return {
root: root
};
})();
})();
