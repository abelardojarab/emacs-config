

(function (globals) {

  var django = globals.django || (globals.django = {});

  
  django.pluralidx = function (n) {
    var v=0;
    if (typeof(v) == 'boolean') {
      return v ? 1 : 0;
    } else {
      return v;
    }
  };
  

  
  /* gettext library */

  django.catalog = {
    "Add more details": "\u52a0\u4e0a\u66f4\u591a\u7d30\u7bc0", 
    "Add or improve examples": "\u52a0\u5165\u6216\u6539\u5584\u7bc4\u672c", 
    "Article Title Lookup / Link Text": "\u641c\u5c0b\u6587\u4ef6\u6a19\u984c / \u93c8\u7d50\u6587\u5b57", 
    "Aspect ratio": "\u756b\u9762\u6bd4\u4f8b", 
    "Attachments": "\u9644\u4ef6", 
    "Autosave disabled.": "\u5df2\u95dc\u9589\u81ea\u52d5\u5132\u5b58\u3002", 
    "Autosave enabled.": "\u5df2\u958b\u555f\u81ea\u52d5\u5132\u5b58\u3002", 
    "CSS Content": "CSS \u5167\u5bb9", 
    "Choose one...": "\u6311\u9078\u4e00\u500b\u2026", 
    "Close": "\u95dc\u9589", 
    "Close notification": "\u95dc\u9589\u901a\u77e5", 
    "Close submenu": "\u95dc\u9589\u5b50\u9078\u55ae", 
    "Create a Redirect": "\u5efa\u7acb\u91cd\u5c0e", 
    "Default": "\u9810\u8a2d", 
    "Details": "\u8a73\u7d30\u8cc7\u8a0a", 
    "Did this page help you?": "\u9019\u500b\u9801\u9762\u6709\u5e6b\u5230\u60a8\u55ce\uff1f", 
    "Discard the draft": "\u6368\u68c4\u8349\u7a3f", 
    "Document": "\u6587\u4ef6", 
    "Draft": "\u8349\u7a3f", 
    "Embed YouTube Video": "\u5d4c\u5165 YouTube \u5f71\u7247", 
    "Enable autosave.": "\u958b\u555f\u81ea\u52d5\u5132\u5b58\u3002", 
    "Fix incorrect information": "\u4fee\u6b63\u932f\u8aa4\u8cc7\u8a0a", 
    "HTML Content": "HTML \u5167\u5bb9", 
    "How to read CSS syntax.": "\u5982\u4f55\u95b1\u8b80 CSS \u8a9e\u6cd5\u3002", 
    "Insert Code Sample Template": "\u63d2\u5165\u7a0b\u5f0f\u78bc\u7bc4\u4f8b\u6a21\u677f", 
    "Insert Code Sample iFrame": "\u63d2\u5165\u7bc4\u4f8b\u7a0b\u5f0f\u78bc iFrame", 
    "JavaScript Content": "JavaScript \u5167\u5bb9", 
    "Launch": "\u57f7\u884c", 
    "Locate a YouTube Video": "\u5c0b\u627e YouTube \u5f71\u7247", 
    "MDN Redirect": "MDN \u91cd\u5c0e", 
    "Make explanations clearer": "\u89e3\u91cb\u5f97\u66f4\u6e05\u695a\u4e00\u9ede", 
    "More about the beta.": "\u4e86\u89e3 beta \u6e2c\u8a66\u7684\u66f4\u591a\u8cc7\u8a0a\u3002", 
    "My search should have led to a different article": "\u6211\u641c\u5c0b\u7684\u6771\u897f\u8ddf\u9019\u9801\u7121\u95dc", 
    "Never ask me again": "\u4e0d\u8981\u518d\u554f\u6211", 
    "New compatibility tables are in beta ": "\u65b0\u7248\u76f8\u5bb9\u6027\u8868\u683c\u6b63\u5728 beta \u6e2c\u8a66\u4e2d", 
    "New interest...": "\u65b0\u8208\u8da3\u2026", 
    "New tag...": "\u65b0\u589e\u6a19\u7c64\u2026", 
    "No": "\u6c92\u6709", 
    "No Highlight": "\u4e0d\u5f37\u8abf", 
    "No attachments available": "\u6c92\u6709\u9644\u4ef6", 
    "No selection": "\u7121\u9078\u64c7", 
    "Open": "\u958b\u555f", 
    "Open implementation notes": "\u6253\u958b\u5be6\u4f5c\u7b46\u8a18", 
    "Paste YouTube Video URL": "\u8cbc\u4e0a YouTube \u5f71\u7247\u7db2\u5740", 
    "Report an error.": "\u56de\u5831\u932f\u8aa4\u3002", 
    "Reported. Thanks!": "\u5df2\u56de\u5831\uff0c\u611f\u8b1d\u60a8\uff01", 
    "Restore the draft content": "\u9084\u539f\u8349\u7a3f\u5167\u5bb9", 
    "Return to compatibility table.": "\u56de\u5230\u76f8\u5bb9\u6027\u8868\u683c\u3002", 
    "Sample CSS Content": "CSS \u7bc4\u4f8b\u5167\u5bb9", 
    "Sample Finder": "\u7bc4\u4f8b\u641c\u5c0b\u5668", 
    "Sample HTML Content": "HTML \u7bc4\u4f8b\u5167\u5bb9", 
    "Sample JavaScript Content": "JavaScript \u7bc4\u4f8b\u5167\u5bb9", 
    "Save Draft": "\u5132\u5b58\u8349\u7a3f", 
    "Search Stack Overflow": "\u641c\u5c0b Stack Overflow", 
    "Sections in Document": "\u6587\u4ef6\u4e2d\u7684\u6bb5\u843d", 
    "Select a section": "\u9078\u64c7\u6bb5\u843d", 
    "Select an attachment": "\u9078\u64c7\u9644\u4ef6", 
    "Selected: ": "\u5df2\u9078\u64c7:", 
    "Show old table.": "\u986f\u793a\u820a\u7248\u8868\u683c\u3002", 
    "Something else": "\u5176\u4ed6", 
    "Syntax Highlighter": "\u8a9e\u6cd5\u5f37\u8abf", 
    "Take the survey": "\u586b\u5beb\u554f\u5377", 
    "Thanks! We'll fix it.": "\u611f\u8b1d\uff01\u6211\u5011\u6703\u76e1\u5feb\u4fee\u5fa9\u3002", 
    "The URL you've entered doesn't appear to be valid": "\u60a8\u8f38\u5165\u7684\u7db2\u5740\u770b\u8d77\u4f86\u4e0d\u6b63\u78ba", 
    "Translate it into my language": "\u7ffb\u8b6f\u6210\u6211\u7684\u8a9e\u8a00", 
    "URL": "\u7db2\u5740", 
    "Uh oh. What would make it better?": "\u5594\u5594\uff0c\u6709\u4ec0\u9ebc\u53ef\u4ee5\u505a\u5f97\u66f4\u597d\u7684\u5730\u65b9\u55ce\uff1f", 
    "What should the sample title be?": "\u7bc4\u4f8b\u7684\u6a19\u984c\u662f\u4ec0\u9ebc\uff1f", 
    "Yes": "\u6709", 
    "You have a draft from:": "\u4f60\u6709\u500b\u8349\u7a3f\u5f9e\uff1a", 
    "You must input a valid YouTube video URL.": "\u60a8\u5fc5\u9808\u8f38\u5165\u6709\u6548\u7684 YouTube \u5f71\u7247\u7db2\u5740\u3002", 
    "Your browser does not support MathML. A CSS fallback has been used instead.": "\u60a8\u7684\u700f\u89bd\u5668\u4e0d\u652f\u63f4 MathML\uff0c\u5df2\u6539\u7528 CSS \u986f\u793a\u3002", 
    "an unknown date": "\u672a\u77e5\u65e5\u671f", 
    "discarded": "\u5df2\u6368\u68c4", 
    "published": "\u5df2\u767c\u5e03", 
    "restored": "\u5df2\u9084\u539f"
  };

  django.gettext = function (msgid) {
    var value = django.catalog[msgid];
    if (typeof(value) == 'undefined') {
      return msgid;
    } else {
      return (typeof(value) == 'string') ? value : value[0];
    }
  };

  django.ngettext = function (singular, plural, count) {
    var value = django.catalog[singular];
    if (typeof(value) == 'undefined') {
      return (count == 1) ? singular : plural;
    } else {
      return value[django.pluralidx(count)];
    }
  };

  django.gettext_noop = function (msgid) { return msgid; };

  django.pgettext = function (context, msgid) {
    var value = django.gettext(context + '\x04' + msgid);
    if (value.indexOf('\x04') != -1) {
      value = msgid;
    }
    return value;
  };

  django.npgettext = function (context, singular, plural, count) {
    var value = django.ngettext(context + '\x04' + singular, context + '\x04' + plural, count);
    if (value.indexOf('\x04') != -1) {
      value = django.ngettext(singular, plural, count);
    }
    return value;
  };
  

  django.interpolate = function (fmt, obj, named) {
    if (named) {
      return fmt.replace(/%\(\w+\)s/g, function(match){return String(obj[match.slice(2,-2)])});
    } else {
      return fmt.replace(/%s/g, function(match){return String(obj.shift())});
    }
  };


  /* formatting library */

  django.formats = {
    "DATETIME_FORMAT": "Y\u5e74n\u6708j\u65e5 H:i", 
    "DATETIME_INPUT_FORMATS": [
      "%Y/%m/%d %H:%M", 
      "%Y-%m-%d %H:%M", 
      "%Y\u5e74%n\u6708%j\u65e5 %H:%M", 
      "%Y/%m/%d %H:%M:%S", 
      "%Y-%m-%d %H:%M:%S", 
      "%Y\u5e74%n\u6708%j\u65e5 %H:%M:%S", 
      "%Y/%m/%d %H:%M:%S.%f", 
      "%Y-%m-%d %H:%M:%S.%f", 
      "%Y\u5e74%n\u6708%j\u65e5 %H:%n:%S.%f", 
      "%Y-%m-%d"
    ], 
    "DATE_FORMAT": "Y\u5e74n\u6708j\u65e5", 
    "DATE_INPUT_FORMATS": [
      "%Y/%m/%d", 
      "%Y-%m-%d", 
      "%Y\u5e74%n\u6708%j\u65e5"
    ], 
    "DECIMAL_SEPARATOR": ".", 
    "FIRST_DAY_OF_WEEK": "1", 
    "MONTH_DAY_FORMAT": "m\u6708j\u65e5", 
    "NUMBER_GROUPING": "4", 
    "SHORT_DATETIME_FORMAT": "Y\u5e74n\u6708j\u65e5 H:i", 
    "SHORT_DATE_FORMAT": "Y\u5e74n\u6708j\u65e5", 
    "THOUSAND_SEPARATOR": "", 
    "TIME_FORMAT": "H:i", 
    "TIME_INPUT_FORMATS": [
      "%H:%M", 
      "%H:%M:%S", 
      "%H:%M:%S.%f"
    ], 
    "YEAR_MONTH_FORMAT": "Y\u5e74n\u6708"
  };

  django.get_format = function (format_type) {
    var value = django.formats[format_type];
    if (typeof(value) == 'undefined') {
      return format_type;
    } else {
      return value;
    }
  };

  /* add to global namespace */
  globals.pluralidx = django.pluralidx;
  globals.gettext = django.gettext;
  globals.ngettext = django.ngettext;
  globals.gettext_noop = django.gettext_noop;
  globals.pgettext = django.pgettext;
  globals.npgettext = django.npgettext;
  globals.interpolate = django.interpolate;
  globals.get_format = django.get_format;

}(this));

