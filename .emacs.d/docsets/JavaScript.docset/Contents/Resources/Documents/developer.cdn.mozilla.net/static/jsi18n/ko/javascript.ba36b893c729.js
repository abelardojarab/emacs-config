

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
    "Add more details": "\uc138\ubd80 \uc124\uba78 \ucd94\uac00", 
    "Add or improve examples": "\uc608\uc81c \ucd94\uac00 \ub610\ub294 \ud5a5\uc0c1", 
    "Article Title Lookup / Link Text": "\ubb38\uc11c \uc81c\ubaa9 \ubcf4\uae30 / \ud14d\uc2a4\ud2b8 \ub9c1\ud06c", 
    "Aspect ratio": "\uac00\ub85c \uc138\ub85c \ube44\uc728", 
    "Attachments": "\ucca8\ubd80 \ud30c\uc77c", 
    "CSS Content": "CSS \ucf58\ud150\uce20", 
    "Choose one...": "\ud558\ub098\ub97c \uc120\ud0dd\ud558\uc138\uc694...", 
    "Close": "\ub2eb\uae30", 
    "Close notification": "\uc54c\ub9bc \ub2eb\uae30", 
    "Close submenu": "\ud558\uc704 \uba54\ub274 \ub2eb\uae30", 
    "Create a Redirect": "\uc790\ub3d9 \uc774\ub3d9 \ub9cc\ub4e4\uae30", 
    "Default": "\uae30\ubcf8 \uc124\uc815", 
    "Details": "\uc0c1\uc138 \uc815\ubcf4", 
    "Did this page help you?": "\uc774 \ud398\uc774\uc9c0\uac00 \ub3c4\uc6c0\uc774 \ub418\uc5c8\ub098\uc694?", 
    "Discard the draft": "\ucd08\uc548 \uc0ad\uc81c", 
    "Document": "\ubb38\uc11c", 
    "Embed YouTube Video": "YouTube \ube44\ub514\uc624 \uc0bd\uc785", 
    "Fix incorrect information": "\uc798\ubabb\ub41c \uc815\ubcf4 \uc218\uc815", 
    "HTML Content": "HTML \ucf58\ud150\uce20", 
    "How to read CSS syntax.": "CSS \uad6c\ubb38\uc744 \uc77d\ub294 \ubc29\ubc95", 
    "Insert Code Sample Template": "\ucf54\ub4dc \uc0d8\ud50c \uc11c\uc2dd \uc0bd\uc785", 
    "Insert Code Sample iFrame": "\ucf54\ub4dc \uc0d8\ud50c iFrame \uc0bd\uc785", 
    "JavaScript Content": "JavaScript \ucf58\ud150\uce20", 
    "Launch": "\uc2e4\ud589", 
    "Locate a YouTube Video": "YouTube \ube44\ub514\uc624 \uc704\uce58 \uc9c0\uc815", 
    "MDN Redirect": "MDN \uc790\ub3d9 \uc774\ub3d9", 
    "Make explanations clearer": "\uc124\uba85\uc744 \ub354 \uba85\ud655\ud558\uac8c \ud558\uae30", 
    "More about the beta.": "\ubca0\ud0c0 \uc790\uc138\ud788 \ubcf4\uae30", 
    "My search should have led to a different article": "\uac80\uc0c9 \ub0b4\uc6a9\uacfc \ub2e4\ub978 \ubb38\uc11c\uc784", 
    "Never ask me again": "\ub2e4\uc2dc \ubb3b\uc9c0 \uc54a\uc74c", 
    "New compatibility tables are in beta ": "\uc0c8\ub85c\uc6b4 \ud638\ud658\uc131 \ud14c\uc774\ube14\uc740 \ubca0\ud0c0 \ubc84\uc804\uc5d0 \uc788\uc2b5\ub2c8\ub2e4 ", 
    "New interest...": "\uc0c8\ub85c\uc6b4 \uad00\uc2ec\uc0ac...", 
    "New tag...": "\uc0c8 \ud0dc\uadf8...", 
    "No": "\uc544\ub2c8\uc624", 
    "No Highlight": "\uac15\uc870 \uc5c6\uc74c", 
    "No attachments available": "\uc0ac\uc6a9 \uac00\ub2a5\ud55c \ucca8\ubd80 \ud30c\uc77c\uc774 \uc5c6\uc74c", 
    "No selection": "\uc120\ud0dd\ud55c \ud56d\ubaa9 \uc5c6\uc74c", 
    "Open": "\uc5f4\uae30", 
    "Open implementation notes": "\uad6c\ud604 \ub178\ud2b8 \uc5f4\uae30", 
    "Paste YouTube Video URL": "YouTube \ube44\ub514\uc624 URL \ubd99\uc5ec\ub123\uae30", 
    "Report an error.": "\uc624\ub958\ub97c \ubcf4\uace0\ud569\ub2c8\ub2e4.", 
    "Reported. Thanks!": "\ubcf4\uace0\ub418\uc5c8\uc2b5\ub2c8\ub2e4. \uac10\uc0ac\ud569\ub2c8\ub2e4!", 
    "Restore the draft content": "\ucd08\uc548 \ucf58\ud150\uce20\ub97c \ubcf5\uc6d0", 
    "Return to compatibility table.": "\ud638\ud658\uc131 \ud14c\uc774\ube14\ub85c \ub3cc\uc544\uac11\ub2c8\ub2e4.", 
    "Sample CSS Content": "CSS \ucf58\ud150\uce20 \uc608\uc81c", 
    "Sample Finder": "\uc0d8\ud50c \ucc3e\uae30", 
    "Sample HTML Content": "HTML \ucf58\ud150\uce20 \uc608\uc81c", 
    "Sample JavaScript Content": "JavaScript \ucf58\ud150\uce20 \uc608\uc81c", 
    "Search Stack Overflow": "\uc2a4\ud0dd \uc624\ubc84\ud50c\ub85c\uc6b0 \uac80\uc0c9", 
    "Sections in Document": "\ubb38\uc11c \ub0b4 \uc139\uc158", 
    "Select a section": "\uc139\uc158 \uc120\ud0dd", 
    "Select an attachment": "\ucca8\ubd80 \ud30c\uc77c \uc120\ud0dd", 
    "Selected: ": "\uc120\ud0dd\ub428: ", 
    "Show old table.": "\uc774\uc804 \ud45c\ub97c \ud45c\uc2dc\ud569\ub2c8\ub2e4.", 
    "Something else": "\ub098\uba38\uc9c0", 
    "Syntax Highlighter": "\uad6c\ubb38 \uac15\uc870", 
    "Take the survey": "\uc124\ubb38 \uc870\uc0ac\uc5d0 \ucc38\uc5ec\ud574\uc8fc\uc138\uc694.", 
    "Thanks! We'll fix it.": "\uac10\uc0ac\ud569\ub2c8\ub2e4! \uace7 \uc218\uc815\ud558\uaca0\uc2b5\ub2c8\ub2e4.", 
    "The URL you've entered doesn't appear to be valid": "\uc785\ub825\ud55c URL\uc774 \uc720\ud6a8\ud558\uac8c \ubcf4\uc774\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4", 
    "Translate it into my language": "\ub0b4 \uc5b8\uc5b4\ub85c \ubc88\uc5ed", 
    "URL": "URL", 
    "Uh oh. What would make it better?": "\uc6b0\uc640. \ub354 \ub098\uc740 \ubc29\ubc95\uc774 \ubb34\uc5c7\uc778\uac00\uc694?", 
    "What should the sample title be?": "\uc0d8\ud50c \uc81c\ubaa9\uc744 \ubb34\uc5c7\uc73c\ub85c \ud560\uae4c\uc694?", 
    "Yes": "\uc608", 
    "You have a draft from:": "\ubcf4\uc874\ub41c \ucd08\uc548\uc774 \uc788\uc2b5\ub2c8\ub2e4:", 
    "You must input a valid YouTube video URL.": "\uc720\ud6a8\ud55c YouTube \ube44\ub514\uc624 URL\uc744 \uc785\ub825\ud574\uc57c \ud569\ub2c8\ub2e4.", 
    "Your browser does not support MathML. A CSS fallback has been used instead.": "\ube0c\ub77c\uc6b0\uc800\uac00 MathML\uc744 \uc9c0\uc6d0\ud558\uc9c0 \uc54a\uc2b5\ub2c8\ub2e4. CSS \ud3f4\ubc31\uc774 \ub300\uc2e0 \uc0ac\uc6a9\ub418\uc5c8\uc2b5\ub2c8\ub2e4.", 
    "an unknown date": "\uc54c \uc218 \uc5c6\ub294 \ub0a0\uc9dc"
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
    "DATETIME_FORMAT": "Y\ub144 n\uc6d4 j\uc77c g:i A", 
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
      "%m/%d/%y", 
      "%Y\ub144 %m\uc6d4 %d\uc77c %H\uc2dc %M\ubd84 %S\ucd08", 
      "%Y\ub144 %m\uc6d4 %d\uc77c %H\uc2dc %M\ubd84"
    ], 
    "DATE_FORMAT": "Y\ub144 n\uc6d4 j\uc77c", 
    "DATE_INPUT_FORMATS": [
      "%Y-%m-%d", 
      "%m/%d/%Y", 
      "%m/%d/%y", 
      "%Y\ub144 %m\uc6d4 %d\uc77c"
    ], 
    "DECIMAL_SEPARATOR": ".", 
    "FIRST_DAY_OF_WEEK": "0", 
    "MONTH_DAY_FORMAT": "F\uc6d4 j\uc77c", 
    "NUMBER_GROUPING": "3", 
    "SHORT_DATETIME_FORMAT": "Y-n-j H:i", 
    "SHORT_DATE_FORMAT": "Y-n-j.", 
    "THOUSAND_SEPARATOR": ",", 
    "TIME_FORMAT": "A g:i", 
    "TIME_INPUT_FORMATS": [
      "%H:%M:%S", 
      "%H:%M:%S.%f", 
      "%H:%M", 
      "%H\uc2dc %M\ubd84 %S\ucd08", 
      "%H\uc2dc %M\ubd84"
    ], 
    "YEAR_MONTH_FORMAT": "Y\ub144 F\uc6d4"
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

