

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
    "Add more details": "\u3055\u3089\u306a\u308b\u8a73\u7d30\u3092\u8ffd\u52a0\u3059\u308b", 
    "Add or improve examples": "\u4f8b\u3092\u8ffd\u52a0\u3082\u3057\u304f\u306f\u6539\u5584\u3059\u308b", 
    "Article Title Lookup / Link Text": "\u8a18\u4e8b\u30bf\u30a4\u30c8\u30eb\u691c\u7d22 / \u30ea\u30f3\u30af\u30c6\u30ad\u30b9\u30c8", 
    "Aspect ratio": "\u7e26\u6a2a\u6bd4", 
    "Attachments": "\u6dfb\u4ed8\u30d5\u30a1\u30a4\u30eb", 
    "Autosave disabled.": "\u81ea\u52d5\u4fdd\u5b58\u304c\u7121\u52b9\u5316\u3055\u308c\u307e\u3057\u305f\u3002", 
    "Autosave enabled.": "\u81ea\u52d5\u4fdd\u5b58\u304c\u6709\u52b9\u5316\u3055\u308c\u307e\u3057\u305f\u3002", 
    "CSS Content": "CSS \u30b3\u30f3\u30c6\u30f3\u30c4", 
    "Choose one...": "\u3072\u3068\u3064\u9078\u629e\u3057\u3066\u304f\u3060\u3055\u3044...", 
    "Close": "\u9589\u3058\u308b", 
    "Close notification": "\u901a\u77e5\u3092\u9589\u3058\u308b", 
    "Close submenu": "\u30b5\u30d6\u30e1\u30cb\u30e5\u30fc\u3092\u9589\u3058\u308b", 
    "Create a Redirect": "\u30ea\u30c0\u30a4\u30ec\u30af\u30c8\u3092\u4f5c\u6210", 
    "Default": "\u65e2\u5b9a\u5024", 
    "Details": "\u8a73\u7d30", 
    "Did this page help you?": "\u3053\u306e\u30da\u30fc\u30b8\u306f\u304a\u5f79\u306b\u7acb\u3061\u307e\u3057\u305f\u304b\uff1f", 
    "Discard the draft": "\u8349\u7a3f\u3092\u524a\u9664", 
    "Document": "\u30c9\u30ad\u30e5\u30e1\u30f3\u30c8", 
    "Draft": "\u8349\u7a3f", 
    "Embed YouTube Video": "YouTube \u52d5\u753b\u3092\u57cb\u3081\u8fbc\u307f", 
    "Enable autosave.": "\u81ea\u52d5\u4fdd\u5b58\u3092\u6709\u52b9\u5316", 
    "Fix incorrect information": "\u9593\u9055\u3063\u305f\u60c5\u5831\u3092\u8a02\u6b63\u3059\u308b", 
    "HTML Content": "HTML \u30b3\u30f3\u30c6\u30f3\u30c4", 
    "How to read CSS syntax.": "CSS \u69cb\u6587\u306e\u8aad\u307f\u65b9", 
    "Insert Code Sample Template": "\u30b3\u30fc\u30c9\u30b5\u30f3\u30d7\u30eb\u30c6\u30f3\u30d7\u30ec\u30fc\u30c8\u3092\u633f\u5165", 
    "Insert Code Sample iFrame": "\u30b3\u30fc\u30c9\u30b5\u30f3\u30d7\u30eb\u306e iFrame \u3092\u633f\u5165", 
    "JavaScript Content": "JavaScript \u30b3\u30f3\u30c6\u30f3\u30c4", 
    "Launch": "\u958b\u304f", 
    "Locate a YouTube Video": "YouTube \u52d5\u753b\u3092\u7279\u5b9a", 
    "MDN Redirect": "MDN \u30ea\u30c0\u30a4\u30ec\u30af\u30c8", 
    "Make explanations clearer": "\u8aac\u660e\u3092\u3088\u308a\u660e\u78ba\u306b\u3059\u308b", 
    "More about the beta.": "\u30d9\u30fc\u30bf\u7248\u306b\u95a2\u3059\u308b\u8a73\u7d30", 
    "My search should have led to a different article": "\u691c\u7d22\u7d50\u679c\u304b\u3089\u5225\u306e\u8a18\u4e8b\u306b\u305f\u3069\u308a\u7740\u304d\u307e\u3057\u305f", 
    "Never ask me again": "\u518d\u5ea6\u8868\u793a\u3057\u306a\u3044", 
    "New compatibility tables are in beta ": "\u65b0\u3057\u3044\u4e92\u63db\u6027\u306e\u8868\u306f\u30d9\u30fc\u30bf\u7248\u3067\u3059\u00a0", 
    "New interest...": "\u65b0\u305f\u306a\u8208\u5473...", 
    "New tag...": "\u65b0\u3057\u3044\u30bf\u30b0...", 
    "No": "\u3044\u3044\u3048", 
    "No Highlight": "\u30cf\u30a4\u30e9\u30a4\u30c8\u306a\u3057", 
    "No attachments available": "\u8868\u793a\u53ef\u80fd\u306a\u6dfb\u4ed8\u30d5\u30a1\u30a4\u30eb\u306f\u3042\u308a\u307e\u305b\u3093", 
    "No selection": "\u9078\u629e\u306a\u3057", 
    "Open": "\u958b\u304f", 
    "Open implementation notes": "\u5b9f\u88c5\u30e1\u30e2\u3092\u958b\u304f", 
    "Paste YouTube Video URL": "YouTube \u52d5\u753b\u306e URL \u3092\u8cbc\u308a\u4ed8\u3051", 
    "Report an error.": "\u554f\u984c\u3092\u5831\u544a", 
    "Reported. Thanks!": "\u5831\u544a\u304c\u9001\u4fe1\u3055\u308c\u307e\u3057\u305f\u3002\u3042\u308a\u304c\u3068\u3046\u3054\u3056\u3044\u307e\u3059\uff01", 
    "Restore the draft content": "\u8349\u7a3f\u306e\u5185\u5bb9\u3092\u5fa9\u5143", 
    "Return to compatibility table.": "\u4e92\u63db\u6027\u306e\u8868\u3078\u623b\u308b", 
    "Sample CSS Content": "\u30b5\u30f3\u30d7\u30eb CSS \u30b3\u30f3\u30c6\u30f3\u30c4", 
    "Sample Finder": "\u30b5\u30f3\u30d7\u30eb\u30d5\u30a1\u30a4\u30f3\u30c0\u30fc", 
    "Sample HTML Content": "\u30b5\u30f3\u30d7\u30eb HTML \u30b3\u30f3\u30c6\u30f3\u30c4", 
    "Sample JavaScript Content": "\u30b5\u30f3\u30d7\u30eb JavaScript \u30b3\u30f3\u30c6\u30f3\u30c4", 
    "Save Draft": "\u8349\u7a3f\u3092\u4fdd\u5b58", 
    "Search Stack Overflow": "Stack Overflow \u3092\u691c\u7d22", 
    "Sections in Document": "\u30c9\u30ad\u30e5\u30e1\u30f3\u30c8\u5185\u306e\u30bb\u30af\u30b7\u30e7\u30f3", 
    "Select a section": "\u30bb\u30af\u30b7\u30e7\u30f3\u3092\u9078\u629e", 
    "Select an attachment": "\u6dfb\u4ed8\u30d5\u30a1\u30a4\u30eb\u3092\u9078\u629e", 
    "Selected: ": "\u9078\u629e\u6e08\u307f: ", 
    "Show old table.": "\u53e4\u3044\u8868\u3092\u8868\u793a", 
    "Something else": "\u305d\u306e\u4ed6", 
    "Syntax Highlighter": "\u69cb\u6587\u30cf\u30a4\u30e9\u30a4\u30bf\u30fc", 
    "Take the survey": "\u30a2\u30f3\u30b1\u30fc\u30c8\u306b\u7b54\u3048\u308b", 
    "Thanks! We'll fix it.": "\u3042\u308a\u304c\u3068\u3046\u3054\u3056\u3044\u307e\u3059\u3002\u3053\u306e\u554f\u984c\u3092\u4fee\u6b63\u3057\u307e\u3059\u3002", 
    "The URL you've entered doesn't appear to be valid": "\u5165\u529b\u3055\u308c\u305f URL \u304c\u6b63\u3057\u304f\u306a\u3044\u3088\u3046\u3067\u3059", 
    "Translate it into my language": "\u81ea\u5206\u306e\u8a00\u8a9e\u306b\u7ffb\u8a33", 
    "URL": "URL", 
    "Uh oh. What would make it better?": "\u7533\u3057\u8a33\u3042\u308a\u307e\u305b\u3093\u3002\u3069\u3046\u3057\u305f\u3089\u6539\u5584\u3067\u304d\u308b\u3067\u3057\u3087\u3046\u304b\uff1f", 
    "What should the sample title be?": "\u30b5\u30f3\u30d7\u30eb\u306e\u30bf\u30a4\u30c8\u30eb\u3092\u8a18\u5165\u3057\u3066\u304f\u3060\u3055\u3044\u3002", 
    "Yes": "\u306f\u3044", 
    "You have a draft from:": "\u3053\u306e\u65e5\u6642\u306b\u4fdd\u5b58\u3055\u308c\u305f\u8349\u7a3f\u304c\u3042\u308a\u307e\u3059:", 
    "You must input a valid YouTube video URL.": "\u6b63\u3057\u3044 YouTube \u52d5\u753b\u306e URL \u3092\u5165\u529b\u3057\u3066\u304f\u3060\u3055\u3044\u3002", 
    "Your browser does not support MathML. A CSS fallback has been used instead.": "\u304a\u4f7f\u3044\u306e\u30d6\u30e9\u30a6\u30b6\u306f MathML \u306b\u5bfe\u5fdc\u3057\u3066\u3044\u307e\u305b\u3093\u3002\u4ee3\u308f\u308a\u306b CSS \u30d5\u30a9\u30fc\u30eb\u30d0\u30c3\u30af\u3092\u8868\u793a\u3057\u307e\u3059\u3002", 
    "an unknown date": "\u4e0d\u660e\u306a\u65e5\u6642", 
    "discarded": "\u7834\u68c4\u3055\u308c\u307e\u3057\u305f", 
    "published": "\u516c\u958b\u6e08\u307f", 
    "restored": "\u5fa9\u5143\u3055\u308c\u305f\u5185\u5bb9"
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
    "DATETIME_FORMAT": "Y\u5e74n\u6708j\u65e5G:i", 
    "DATETIME_INPUT_FORMATS": [
      "%Y-%m-%d %H:%M:%S", 
      "%Y-%m-%d %H:%M:%S.%f", 
      "%Y-%m-%d %H:%M", 
      "%Y-%m-%d", 
      "%m/%d/%Y %H:%M:%S", 
      "%m/%d/%Y %H:%M:%S.%f", 
      "%m/%d/%Y %H:%M", 
      "%m/%d/%Y", 
      "%m/%d/%y %H:%M:%S", 
      "%m/%d/%y %H:%M:%S.%f", 
      "%m/%d/%y %H:%M", 
      "%m/%d/%y"
    ], 
    "DATE_FORMAT": "Y\u5e74n\u6708j\u65e5", 
    "DATE_INPUT_FORMATS": [
      "%Y-%m-%d", 
      "%m/%d/%Y", 
      "%m/%d/%y", 
      "%b %d %Y", 
      "%b %d, %Y", 
      "%d %b %Y", 
      "%d %b, %Y", 
      "%B %d %Y", 
      "%B %d, %Y", 
      "%d %B %Y", 
      "%d %B, %Y"
    ], 
    "DECIMAL_SEPARATOR": ".", 
    "FIRST_DAY_OF_WEEK": "0", 
    "MONTH_DAY_FORMAT": "n\u6708j\u65e5", 
    "NUMBER_GROUPING": "0", 
    "SHORT_DATETIME_FORMAT": "Y/m/d G:i", 
    "SHORT_DATE_FORMAT": "Y/m/d", 
    "THOUSAND_SEPARATOR": ",", 
    "TIME_FORMAT": "G:i", 
    "TIME_INPUT_FORMATS": [
      "%H:%M:%S", 
      "%H:%M:%S.%f", 
      "%H:%M"
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

