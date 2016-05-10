(function() {(window.nunjucksPrecompiled = window.nunjucksPrecompiled || {})["context_menu.html"] = (function() {
function root(env, context, frame, runtime, cb) {
var lineno = null;
var colno = null;
var output = "";
try {
var parentTemplate = null;
output += "<ul id=\"context-menu\" class=\"context-menu\" tabindex=\"0\">\n    ";
frame = frame.push();
var t_3 = runtime.contextOrFrameLookup(context, frame, "menuItems");
if(t_3) {var t_2 = t_3.length;
for(var t_1=0; t_1 < t_3.length; t_1++) {
var t_4 = t_3[t_1];
frame.set("item", t_4);
frame.set("loop.index", t_1 + 1);
frame.set("loop.index0", t_1);
frame.set("loop.revindex", t_2 - t_1);
frame.set("loop.revindex0", t_2 - t_1 - 1);
frame.set("loop.first", t_1 === 0);
frame.set("loop.last", t_1 === t_2 - 1);
frame.set("loop.length", t_2);
output += "\n        <li><a href=\"";
output += runtime.suppressValue(runtime.memberLookup((t_4),"href"), env.opts.autoescape);
output += "\" class=\"";
output += runtime.suppressValue(runtime.memberLookup((t_4),"icon"), env.opts.autoescape);
output += " icon\">";
output += runtime.suppressValue(env.getFilter("safe").call(context, env.getFilter("default").call(context, runtime.memberLookup((t_4),"html"),runtime.memberLookup((t_4),"text"))), env.opts.autoescape);
output += "</a></li>\n    ";
;
}
}
frame = frame.pop();
output += "\n</ul>\n";
if(parentTemplate) {
parentTemplate.rootRenderFunc(env, context, frame, runtime, cb);
} else {
cb(null, output);
}
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
(function() {(window.nunjucksPrecompiled = window.nunjucksPrecompiled || {})["path_line.html"] = (function() {
function root(env, context, frame, runtime, cb) {
var lineno = null;
var colno = null;
var output = "";
try {
var parentTemplate = null;
if(!runtime.contextOrFrameLookup(context, frame, "is_first_or_only")) {
output += "<span class=\"path-separator\">/</span>";
;
}
if(!runtime.contextOrFrameLookup(context, frame, "is_binary") || runtime.contextOrFrameLookup(context, frame, "is_dir")) {
output += "<a href=\"";
output += runtime.suppressValue(runtime.contextOrFrameLookup(context, frame, "url"), env.opts.autoescape);
output += "\"";
if(runtime.contextOrFrameLookup(context, frame, "is_dir")) {
output += " data-path=\"";
output += runtime.suppressValue(runtime.contextOrFrameLookup(context, frame, "data_path"), env.opts.autoescape);
output += "\" ";
;
}
output += ">";
;
}
output += runtime.suppressValue(runtime.contextOrFrameLookup(context, frame, "display_path"), env.opts.autoescape);
if(!runtime.contextOrFrameLookup(context, frame, "is_binary") || runtime.contextOrFrameLookup(context, frame, "is_dir")) {
output += "</a>";
;
}
if(parentTemplate) {
parentTemplate.rootRenderFunc(env, context, frame, runtime, cb);
} else {
cb(null, output);
}
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
(function() {(window.nunjucksPrecompiled = window.nunjucksPrecompiled || {})["result_lines.html"] = (function() {
function root(env, context, frame, runtime, cb) {
var lineno = null;
var colno = null;
var output = "";
try {
var parentTemplate = null;
var macro_t_1 = runtime.makeMacro(
["result", "www_root", "tree"], 
[], 
function (l_result, l_www_root, l_tree, kwargs) {
frame = frame.push();
kwargs = kwargs || {};
if (kwargs.hasOwnProperty("caller")) {
frame.set("caller", kwargs.caller); }
frame.set("result", l_result);
frame.set("www_root", l_www_root);
frame.set("tree", l_tree);
var t_2 = "";frame = frame.push();
var t_5 = runtime.memberLookup((l_result),"lines");
if(t_5) {var t_4 = t_5.length;
for(var t_3=0; t_3 < t_5.length; t_3++) {
var t_6 = t_5[t_3];
frame.set("entry", t_6);
frame.set("loop.index", t_3 + 1);
frame.set("loop.index0", t_3);
frame.set("loop.revindex", t_4 - t_3);
frame.set("loop.revindex0", t_4 - t_3 - 1);
frame.set("loop.first", t_3 === 0);
frame.set("loop.last", t_3 === t_4 - 1);
frame.set("loop.length", t_4);
t_2 += "\n    <tr>\n      <td class=\"left-column\">\n        <a href=\"";
t_2 += runtime.suppressValue(l_www_root, env.opts.autoescape);
t_2 += "/";
t_2 += runtime.suppressValue(l_tree, env.opts.autoescape);
t_2 += "/source/";
t_2 += runtime.suppressValue(runtime.memberLookup((l_result),"path"), env.opts.autoescape);
t_2 += "#";
t_2 += runtime.suppressValue(runtime.memberLookup((t_6),"line_number"), env.opts.autoescape);
t_2 += "\">\n          ";
t_2 += runtime.suppressValue(runtime.memberLookup((t_6),"line_number"), env.opts.autoescape);
t_2 += "\n        </a>\n      </td>\n      <td>\n        <a href=\"";
t_2 += runtime.suppressValue(l_www_root, env.opts.autoescape);
t_2 += "/";
t_2 += runtime.suppressValue(l_tree, env.opts.autoescape);
t_2 += "/source/";
t_2 += runtime.suppressValue(runtime.memberLookup((l_result),"path"), env.opts.autoescape);
t_2 += "#";
t_2 += runtime.suppressValue(runtime.memberLookup((t_6),"line_number"), env.opts.autoescape);
t_2 += "\">\n          <code aria-labelledby=\"";
t_2 += runtime.suppressValue(runtime.memberLookup((t_6),"line_number"), env.opts.autoescape);
t_2 += "\">";
t_2 += runtime.suppressValue(env.getFilter("safe").call(context, runtime.memberLookup((t_6),"line")), env.opts.autoescape);
t_2 += "</code>\n        </a>\n      </td>\n    </tr>\n  ";
;
}
}
frame = frame.pop();
;
frame = frame.pop();
return new runtime.SafeString(t_2);
});
context.addExport("result_lines");
context.setVariable("result_lines", macro_t_1);
output += "\n\n";
output += "\n";
output += runtime.suppressValue((lineno = 18, colno = 13, runtime.callWrap(macro_t_1, "result_lines", context, [runtime.contextOrFrameLookup(context, frame, "result"),runtime.contextOrFrameLookup(context, frame, "www_root"),runtime.contextOrFrameLookup(context, frame, "tree")])), env.opts.autoescape);
output += "\n";
if(parentTemplate) {
parentTemplate.rootRenderFunc(env, context, frame, runtime, cb);
} else {
cb(null, output);
}
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
(function() {(window.nunjucksPrecompiled = window.nunjucksPrecompiled || {})["results.html"] = (function() {
function root(env, context, frame, runtime, cb) {
var lineno = null;
var colno = null;
var output = "";
try {
var parentTemplate = null;
env.getTemplate("result_lines.html", false, "results.html", function(t_2,t_1) {
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
if (kwargs.hasOwnProperty("caller")) {
frame.set("caller", kwargs.caller); }
frame.set("results", l_results);
frame.set("www_root", l_www_root);
frame.set("tree", l_tree);
var t_6 = "";frame = frame.push();
var t_9 = l_results;
if(t_9) {var t_8 = t_9.length;
for(var t_7=0; t_7 < t_9.length; t_7++) {
var t_10 = t_9[t_7];
frame.set("result", t_10);
frame.set("loop.index", t_7 + 1);
frame.set("loop.index0", t_7);
frame.set("loop.revindex", t_8 - t_7);
frame.set("loop.revindex0", t_8 - t_7 - 1);
frame.set("loop.first", t_7 === 0);
frame.set("loop.last", t_7 === t_8 - 1);
frame.set("loop.length", t_8);
t_6 += "\n    <tbody class=\"result\" data-path=\"";
t_6 += runtime.suppressValue(runtime.memberLookup((t_10),"path"), env.opts.autoescape);
t_6 += "\">\n      <tr class=\"result-head ";
if(runtime.memberLookup((t_10),"is_binary")) {
t_6 += " binary_row ";
;
}
t_6 += "\">\n        <td class=\"left-column\">\n          <div class=\"";
t_6 += runtime.suppressValue(runtime.memberLookup((t_10),"iconClass"), env.opts.autoescape);
t_6 += " icon-container\"></div>\n        </td>\n        <td>\n          ";
t_6 += runtime.suppressValue(env.getFilter("safe").call(context, runtime.memberLookup((t_10),"pathLine")), env.opts.autoescape);
t_6 += "\n        </td>\n      </tr>\n      ";
t_6 += runtime.suppressValue((lineno = 13, colno = 19, runtime.callWrap(t_4, "result_lines", context, [t_10,l_www_root,l_tree])), env.opts.autoescape);
t_6 += "\n    </tbody>\n  ";
;
}
}
frame = frame.pop();
;
frame = frame.pop();
return new runtime.SafeString(t_6);
});
context.addExport("results_list");
context.setVariable("results_list", macro_t_5);
output += "\n\n";
output += "\n";
output += runtime.suppressValue((lineno = 19, colno = 13, runtime.callWrap(macro_t_5, "results_list", context, [runtime.contextOrFrameLookup(context, frame, "results"),runtime.contextOrFrameLookup(context, frame, "www_root"),runtime.contextOrFrameLookup(context, frame, "tree")])), env.opts.autoescape);
output += "\n";
if(parentTemplate) {
parentTemplate.rootRenderFunc(env, context, frame, runtime, cb);
} else {
cb(null, output);
}
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
(function() {(window.nunjucksPrecompiled = window.nunjucksPrecompiled || {})["results_container.html"] = (function() {
function root(env, context, frame, runtime, cb) {
var lineno = null;
var colno = null;
var output = "";
try {
var parentTemplate = null;
env.getTemplate("results.html", false, "results_container.html", function(t_2,t_1) {
if(t_2) { cb(t_2); return; }
t_1.getExported(function(t_3,t_1) {
if(t_3) { cb(t_3); return; }
if(t_1.hasOwnProperty("results_list")) {
var t_4 = t_1.results_list;
} else {
cb(new Error("cannot import 'results_list'")); return;
}
context.setVariable("results_list", t_4);
env.getTemplate("results_header.html", false, "results_container.html", function(t_6,t_5) {
if(t_6) { cb(t_6); return; }
t_5.getExported(function(t_7,t_5) {
if(t_7) { cb(t_7); return; }
if(t_5.hasOwnProperty("results_header")) {
var t_8 = t_5.results_header;
} else {
cb(new Error("cannot import 'results_header'")); return;
}
context.setVariable("results_header", t_8);
output += runtime.suppressValue((lineno = 3, colno = 15, runtime.callWrap(t_8, "results_header", context, [runtime.contextOrFrameLookup(context, frame, "result_count"),runtime.contextOrFrameLookup(context, frame, "result_count_formatted"),runtime.contextOrFrameLookup(context, frame, "top_of_tree"),runtime.contextOrFrameLookup(context, frame, "tree_tuples"),runtime.contextOrFrameLookup(context, frame, "tree")])), env.opts.autoescape);
output += "\n\n";
if(env.getFilter("length").call(context, runtime.contextOrFrameLookup(context, frame, "results")) > 0) {
output += "\n  <table class=\"results\">\n    <caption class=\"visually-hidden\">Query matches</caption>\n    <thead class=\"visually-hidden\">\n      <th scope=\"col\">Line</th>\n      <th scope=\"col\">Code Snippet</th>\n    </thead>\n    ";
output += runtime.suppressValue((lineno = 12, colno = 17, runtime.callWrap(t_4, "results_list", context, [runtime.contextOrFrameLookup(context, frame, "results"),runtime.contextOrFrameLookup(context, frame, "www_root"),runtime.contextOrFrameLookup(context, frame, "tree")])), env.opts.autoescape);
output += "\n  </table>\n";
;
}
output += "\n";
if(parentTemplate) {
parentTemplate.rootRenderFunc(env, context, frame, runtime, cb);
} else {
cb(null, output);
}
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
(function() {(window.nunjucksPrecompiled = window.nunjucksPrecompiled || {})["results_header.html"] = (function() {
function root(env, context, frame, runtime, cb) {
var lineno = null;
var colno = null;
var output = "";
try {
var parentTemplate = null;
env.getTemplate("switch_tree.html", false, "results_header.html", function(t_2,t_1) {
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
if (kwargs.hasOwnProperty("caller")) {
frame.set("caller", kwargs.caller); }
frame.set("result_count", l_result_count);
frame.set("result_count_formatted", l_result_count_formatted);
frame.set("top_of_tree", l_top_of_tree);
frame.set("tree_tuples", l_tree_tuples);
frame.set("tree", l_tree);
var t_6 = "";t_6 += "<p class=\"top-of-tree\">\n    ";
t_6 += runtime.suppressValue(l_result_count_formatted, env.opts.autoescape);
t_6 += " ";
t_6 += runtime.suppressValue((l_result_count == 1?"result":"results"), env.opts.autoescape);
t_6 += " from the <a href=\"";
t_6 += runtime.suppressValue(l_top_of_tree, env.opts.autoescape);
t_6 += "\">";
t_6 += runtime.suppressValue(l_tree, env.opts.autoescape);
t_6 += "</a> tree";
if(l_result_count > 0) {
t_6 += ":";
;
}
t_6 += "\n  </p>\n\n  ";
t_6 += runtime.suppressValue((lineno = 7, colno = 12, runtime.callWrap(t_4, "tree_menu", context, [l_tree_tuples,l_tree])), env.opts.autoescape);
;
frame = frame.pop();
return new runtime.SafeString(t_6);
});
context.addExport("results_header");
context.setVariable("results_header", macro_t_5);
if(parentTemplate) {
parentTemplate.rootRenderFunc(env, context, frame, runtime, cb);
} else {
cb(null, output);
}
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
(function() {(window.nunjucksPrecompiled = window.nunjucksPrecompiled || {})["switch_tree.html"] = (function() {
function root(env, context, frame, runtime, cb) {
var lineno = null;
var colno = null;
var output = "";
try {
var parentTemplate = null;
var macro_t_1 = runtime.makeMacro(
["tree_tuples", "selected_tree"], 
[], 
function (l_tree_tuples, l_selected_tree, kwargs) {
frame = frame.push();
kwargs = kwargs || {};
if (kwargs.hasOwnProperty("caller")) {
frame.set("caller", kwargs.caller); }
frame.set("tree_tuples", l_tree_tuples);
frame.set("selected_tree", l_selected_tree);
var t_2 = "";t_2 += "\n  ";
if(env.getFilter("length").call(context, l_tree_tuples) > 1) {
t_2 += "\n    <section id=\"tree-selector\" class=\"tree-selector\">\n      <button type=\"button\" class=\"ts-select-trigger\" aria-label=\"Switch Tree\">\n        <!-- arrow icon using icon font -->\n        <span aria-hidden=\"true\" data-icon-arrow=\"&#xe801;\" class=\"selector-arrow\">\n          <!-- tree icon using icon font -->\n          <span aria-hidden=\"true\" data-icon=\"&#xe800;\"></span>\n          <span class='current-tree'>Switch Tree</span>\n        </span>\n      </button>\n      <div class=\"select-options ts-modal\" aria-expanded=\"false\">\n        <form name=\"options-filter\" class=\"options-filter\" data-active=\"false\">\n          <label for=\"filter-txt\" class=\"visually-hidden\">Filter Trees</label>\n          <input type=\"text\" name=\"filter-txt\" id=\"filter-txt\" placeholder=\"Filter trees\" />\n          <input type=\"submit\" value=\"Filter\" class=\"visually-hidden\" />\n        </form>\n        <ul class=\"selector-options\" tabindex=\"-1\">\n          ";
frame = frame.push();
var t_5 = l_tree_tuples;
if(t_5) {var t_3;
if(runtime.isArray(t_5)) {
var t_4 = t_5.length;
for(t_3=0; t_3 < t_5.length; t_3++) {
var t_6 = t_5[t_3][0]
frame.set("tree", t_5[t_3][0]);
var t_7 = t_5[t_3][1]
frame.set("url", t_5[t_3][1]);
var t_8 = t_5[t_3][2]
frame.set("description", t_5[t_3][2]);
frame.set("loop.index", t_3 + 1);
frame.set("loop.index0", t_3);
frame.set("loop.revindex", t_4 - t_3);
frame.set("loop.revindex0", t_4 - t_3 - 1);
frame.set("loop.first", t_3 === 0);
frame.set("loop.last", t_3 === t_4 - 1);
frame.set("loop.length", t_4);
t_2 += "\n            <li>\n              <a href=\"";
t_2 += runtime.suppressValue(t_7, env.opts.autoescape);
t_2 += "\" ";
if(t_6 == l_selected_tree) {
t_2 += "class=\"selected\" aria-checked=\"true\"";
;
}
t_2 += ">\n                <span class=\"selector-option-label\">";
t_2 += runtime.suppressValue(t_6, env.opts.autoescape);
t_2 += "</span>\n                <span class=\"selector-option-description\">";
t_2 += runtime.suppressValue(t_8, env.opts.autoescape);
t_2 += "</span>\n              </a>\n            </li>\n          ";
;
}
} else {
t_3 = -1;
var t_4 = runtime.keys(t_5).length;
for(var t_9 in t_5) {
t_3++;
var t_10 = t_5[t_9];
frame.set("tree", t_9);
frame.set("url", t_10);
frame.set("loop.index", t_3 + 1);
frame.set("loop.index0", t_3);
frame.set("loop.revindex", t_4 - t_3);
frame.set("loop.revindex0", t_4 - t_3 - 1);
frame.set("loop.first", t_3 === 0);
frame.set("loop.last", t_3 === t_4 - 1);
frame.set("loop.length", t_4);
t_2 += "\n            <li>\n              <a href=\"";
t_2 += runtime.suppressValue(t_10, env.opts.autoescape);
t_2 += "\" ";
if(t_9 == l_selected_tree) {
t_2 += "class=\"selected\" aria-checked=\"true\"";
;
}
t_2 += ">\n                <span class=\"selector-option-label\">";
t_2 += runtime.suppressValue(t_9, env.opts.autoescape);
t_2 += "</span>\n                <span class=\"selector-option-description\">";
t_2 += runtime.suppressValue(t_8, env.opts.autoescape);
t_2 += "</span>\n              </a>\n            </li>\n          ";
;
}
}
}
frame = frame.pop();
t_2 += "\n        </ul>\n      </div>\n    </section>\n  ";
;
}
t_2 += "\n";
;
frame = frame.pop();
return new runtime.SafeString(t_2);
});
context.addExport("tree_menu");
context.setVariable("tree_menu", macro_t_1);
output += "\n";
if(parentTemplate) {
parentTemplate.rootRenderFunc(env, context, frame, runtime, cb);
} else {
cb(null, output);
}
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

