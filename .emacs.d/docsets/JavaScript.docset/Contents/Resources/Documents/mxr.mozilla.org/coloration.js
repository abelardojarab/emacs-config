document.cookie = 'colorwithjs=1;path=/;max-age=' + (60*60*24*365);

(function(){

var line = 1;
var ml;
var firstTime = true;
window.theCode = [];
window.theCode2 = [];
var currentTags = [];

window.addCode = function (code) {
  if (firstTime) {
    ml = {};
    var tmp = window.marked_lines;
    if (tmp) {
      for (var i=0; i < tmp.length; i++) {
        ml[tmp[i]] = true;
      }
    }
    code = "\n" + code;
  }
  theCode.push (code);
  code = code.replace (
    /<([ACVSI])/g,
    function (str,letter) {
      return "<span class='" + letter.toLowerCase() + "'>";
    } );
  code = code.replace (
    /<([DML])([^>]+)>/g,
    function (str, letter, contents) {
      switch (letter) {
        case "D":
          return "<a class=\"d\" href=\"" + ident_cgi + "?i=" + contents + "\">" +contents + "</a>";
        case "M":
          return "<a href=\"mailto:" + contents + "\">&lt;" + contents + "&gt;</a>";
        case "L":
          return "<a href=\"" + contents + "\">" + contents + "</a>";
      }
      return
    } );

  code = code.replace(
    /<(\/?)([^\s>]+)(\s+[^>]+)?>|(>)/g,
    function (str, isEndTag, tagName, attributesStr, gt) {
      if (!isEndTag && tagName) {
        currentTags.push (tagName);
      } else {
        var string = arguments [arguments.length - 1];
        var offset = arguments [arguments.length - 2];
        var startTag = currentTags.pop();
        if (gt) {
          //throw new Error( gt + " found alone (offset: " + offset + ")\n\n" + string.slice( offset, 150 ) );
          str = "</" + startTag + ">";
        } else if (attributesStr) {
          throw new Error ("Incorrect end tag:\n" + str);
        } else if (startTag != tagName) {
          throw new Error ("Mismatching tags: <" + startTag + "> ... </" + tagName + ">");
        }
      }
      return str;
    } );
  code = code.replace (
    /\n(<div class="m">|<\/div>|<style>[^<]*<\/style>){0,}/g,
    function (str) {
      var l = line++;
      var d = ("" + l).length - 1;
      if (l in ml) {
        d += " m";
      }
      return str + "<a class='l d" + d + "' name=" + l + " href=\"#" + l + "\">" + l + "</a> ";
    } );

  if (firstTime) {
    firstTime = false;
    code = code.slice (1);
  }
  theCode2.push (code);
  document.write (code);
};

})();
