

(function (globals) {

  var django = globals.django || (globals.django = {});

  
  django.pluralidx = function (n) {
    var v=(n != 1);
    if (typeof(v) == 'boolean') {
      return v ? 1 : 0;
    } else {
      return v;
    }
  };
  

  
  /* gettext library */

  django.catalog = {
    "Add more details": "\u0986\u09b0\u0993 \u09ac\u09bf\u09ac\u09b0\u09a3 \u09af\u09cb\u0997 \u0995\u09b0\u09c1\u09a8", 
    "Add or improve examples": "\u0989\u09a6\u09be\u09b9\u09b0\u09a3 \u09af\u09cb\u0997 \u0985\u09a5\u09ac\u09be \u0989\u09a8\u09cd\u09a8\u09a4 \u0995\u09b0\u09c1\u09a8", 
    "Article Title Lookup / Link Text": "\u0986\u09b0\u09cd\u099f\u09bf\u0995\u09c7\u09b2\u09c7\u09b0 \u09b6\u09bf\u09b0\u09cb\u09a8\u09be\u09ae \u0985\u09a8\u09c1\u09b8\u09a8\u09cd\u09a7\u09be\u09a8/\u099f\u09c7\u0995\u09cd\u09b8\u099f \u09b2\u09bf\u0982\u0995", 
    "Attachments": "\u09b8\u0982\u09af\u09c1\u0995\u09cd\u09a4\u09bf", 
    "Choose one...": "\u098f\u0995\u099f\u09bf \u09aa\u099b\u09a8\u09cd\u09a6 \u0995\u09b0\u09c1\u09a8...", 
    "Close": "\u09ac\u09a8\u09cd\u09a7", 
    "Close notification": "\u09a8\u09cb\u099f\u09bf\u09ab\u09bf\u0995\u09c7\u09b6\u09a8 \u09ac\u09a8\u09cd\u09a7 \u0995\u09b0\u09c1\u09a8", 
    "Close submenu": "\u09b8\u09be\u09ac\u09ae\u09c7\u09a8\u09c1 \u09ac\u09a8\u09cd\u09a7 \u0995\u09b0\u09c1\u09a8", 
    "Create a Redirect": "\u098f\u0995\u099f\u09bf \u09aa\u09c1\u09a8\u09a8\u09bf\u09b0\u09cd\u09a6\u09c7\u09b6\u09a8\u09be \u09a4\u09c8\u09b0\u09bf \u0995\u09b0\u09c1\u09a8", 
    "Details": "\u09ac\u09bf\u09b8\u09cd\u09a4\u09be\u09b0\u09bf\u09a4", 
    "Did this page help you?": "\u09aa\u09be\u09a4\u09be\u099f\u09bf \u0995\u09bf \u0986\u09aa\u09a8\u09be\u09b0 \u099c\u09a8\u09cd\u09af \u09b8\u09b9\u09be\u09df\u0995 \u099b\u09bf\u09b2?", 
    "Discard the draft": "\u0996\u09b8\u09a1\u09bc\u09be\u099f\u09bf \u09ac\u09be\u09a4\u09bf\u09b2 \u0995\u09b0", 
    "Fix incorrect information": "\u09ad\u09c1\u09b2 \u09a4\u09a5\u09cd\u09af \u09a0\u09bf\u0995 \u0995\u09b0\u09c1\u09a8", 
    "How to read CSS syntax.": "\u0995\u09bf\u09ad\u09be\u09ac\u09c7 CSS \u09b8\u09bf\u09a8\u099f\u09cd\u09af\u09be\u0995\u09cd\u09b8 \u09aa\u09a1\u09bc\u09a4\u09c7 \u09b9\u09af\u09bc\u0964", 
    "Launch": "\u09b6\u09c1\u09b0\u09c1", 
    "Make explanations clearer": "\u09ac\u09cd\u09af\u09be\u0996\u09cd\u09af\u09be \u0986\u09b0\u0993 \u09b8\u09cd\u09aa\u09b7\u09cd\u099f \u0995\u09b0\u09c1\u09a8", 
    "More about the beta.": "\u09ac\u09c7\u099f\u09be \u09b8\u0982\u09b8\u09cd\u0995\u09b0\u09a3 \u09b8\u09ae\u09cd\u09aa\u09b0\u09cd\u0995\u09c7 \u0986\u09b0\u0993 \u099c\u09be\u09a8\u09c1\u09a8\u0964", 
    "My search should have led to a different article": "\u0986\u09ae\u09be\u09b0 \u0985\u09a8\u09c1\u09b8\u09a8\u09cd\u09a7\u09be\u09a8 \u098f\u0995\u099f\u09bf \u09ad\u09bf\u09a8\u09cd\u09a8 \u09a8\u09bf\u09ac\u09a8\u09cd\u09a7\u09c7 \u09a8\u09bf\u09df\u09c7 \u09af\u09be\u0993\u09df\u09be \u0989\u099a\u09bf\u09a4", 
    "Never ask me again": "\u098f \u09ac\u09bf\u09b7\u09df\u09c7 \u0986\u09b0 \u0995\u0996\u09a8\u0993 \u099c\u09bf\u099c\u09cd\u099e\u09be\u09b8\u09be \u0995\u09b0\u09ac\u09c7 \u09a8\u09be", 
    "New compatibility tables are in beta ": "\u09a8\u09a4\u09c1\u09a8 \u0995\u09ae\u09cd\u09aa\u09cd\u09af\u09be\u099f\u09bf\u09ac\u09bf\u09b2\u09bf\u099f\u09bf \u099f\u09c7\u09ac\u09bf\u09b2\u0997\u09c1\u09b2\u09cb \u09ac\u09c7\u099f\u09be \u09b8\u0982\u09b8\u09cd\u0995\u09b0\u09a3\u09c7 \u0986\u099b\u09c7 ", 
    "New interest...": "\u09a8\u09a4\u09c1\u09a8 \u0986\u0997\u09cd\u09b0\u09b9...", 
    "New tag...": "\u09a8\u09a4\u09c1\u09a8 \u099f\u09cd\u09af\u09be\u0997...", 
    "No": "\u09a8\u09be", 
    "No attachments available": "\u0995\u09cb\u09a8\u09cb \u09b8\u0982\u09af\u09c1\u0995\u09cd\u09a4\u09bf \u09aa\u09be\u0993\u09af\u09bc\u09be \u09af\u09be\u09df\u09a8\u09bf", 
    "No selection": "\u0995\u09cb\u09a8 \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u09a8\u09c7\u0987", 
    "Open": "\u0996\u09c1\u09b2\u09c1\u09a8", 
    "Open implementation notes": "\u0987\u09ae\u09aa\u09cd\u09b2\u09bf\u09ae\u09c7\u09a8\u09cd\u099f\u09c7\u09b6\u09a8 \u09a8\u09cb\u099f \u0996\u09c1\u09b2\u09c1\u09a8", 
    "Report an error.": "\u09a4\u09cd\u09b0\u09c1\u099f\u09bf \u09b0\u09bf\u09aa\u09cb\u09b0\u09cd\u099f \u0995\u09b0\u09c1\u09a8\u0964", 
    "Reported. Thanks!": "\u09b0\u09bf\u09aa\u09cb\u09b0\u09cd\u099f \u09b8\u09ae\u09cd\u09aa\u09a8\u09cd\u09a8\u0964 \u09a7\u09a8\u09cd\u09af\u09ac\u09be\u09a6!", 
    "Restore the draft content": "\u0996\u09b8\u09a1\u09bc\u09be \u0995\u09a8\u09cd\u099f\u09c7\u09a8\u09cd\u099f \u09aa\u09c1\u09a8\u09b0\u09c1\u09a6\u09cd\u09a7\u09be\u09b0 \u0995\u09b0\u09c1\u09a8", 
    "Return to compatibility table.": "\u0995\u09ae\u09cd\u09aa\u09cd\u09af\u09be\u099f\u09bf\u09ac\u09bf\u09b2\u09bf\u099f\u09bf \u099f\u09c7\u09ac\u09bf\u09b2\u09c7 \u09ab\u09bf\u09b0\u09c7 \u09af\u09be\u0993\u0964", 
    "Select an attachment": "\u098f\u0995\u099f\u09bf \u09b8\u0982\u09af\u09c1\u0995\u09cd\u09a4\u09bf \u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09a8 \u0995\u09b0\u09c1\u09a8", 
    "Selected: ": "\u09a8\u09bf\u09b0\u09cd\u09ac\u09be\u099a\u09bf\u09a4: ", 
    "Show old table.": "\u09aa\u09c1\u09b0\u09a8\u09cb \u099f\u09c7\u09ac\u09bf\u09b2 \u09a6\u09c7\u0996\u09c1\u09a8\u0964", 
    "Something else": "\u0985\u09a8\u09cd\u09af \u0995\u09bf\u099b\u09c1", 
    "Take the survey": "\u099c\u09b0\u09bf\u09aa\u09c7 \u0985\u0982\u09b6 \u09a8\u09bf\u09a8", 
    "Thanks! We'll fix it.": "\u09a7\u09a8\u09cd\u09af\u09ac\u09be\u09a6! \u0986\u09ae\u09b0\u09be \u098f\u09b0 \u09b8\u09ae\u09be\u09a7\u09be\u09a8 \u0995\u09b0\u09ac\u0964", 
    "Translate it into my language": "\u0986\u09ae\u09be\u09b0 \u09ad\u09be\u09b7\u09be\u09df \u0985\u09a8\u09c1\u09ac\u09be\u09a6 \u0995\u09b0\u09c1\u09a8", 
    "URL": "URL", 
    "Uh oh. What would make it better?": "\u0986\u09b9..\u09b9\u09be\u0964 \u0986\u09b0\u0993 \u09ad\u09be\u09b2\u09cb \u0995\u09b0\u09be \u09af\u09be\u09df \u0995\u09bf\u09ad\u09be\u09ac\u09c7?", 
    "Yes": "\u09b9\u09cd\u09af\u09be\u0981", 
    "an unknown date": "\u0985\u099c\u09be\u09a8\u09be \u09a4\u09be\u09b0\u09bf\u0996"
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
    "DATETIME_FORMAT": "N j, Y, P", 
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
    "DATE_FORMAT": "j F, Y", 
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
    "MONTH_DAY_FORMAT": "j F", 
    "NUMBER_GROUPING": "0", 
    "SHORT_DATETIME_FORMAT": "m/d/Y P", 
    "SHORT_DATE_FORMAT": "j M, Y", 
    "THOUSAND_SEPARATOR": ",", 
    "TIME_FORMAT": "g:i A", 
    "TIME_INPUT_FORMATS": [
      "%H:%M:%S", 
      "%H:%M:%S.%f", 
      "%H:%M"
    ], 
    "YEAR_MONTH_FORMAT": "F Y"
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

