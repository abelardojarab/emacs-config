

(function (globals) {

  var django = globals.django || (globals.django = {});

  
  django.pluralidx = function (n) {
    var v=(n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);
    if (typeof(v) == 'boolean') {
      return v ? 1 : 0;
    } else {
      return v;
    }
  };
  

  
  /* gettext library */

  django.catalog = {
    "Add more details": "\u0414\u043e\u0434\u0430\u0442\u0438 \u0434\u0435\u0442\u0430\u043b\u0456", 
    "Add or improve examples": "\u0414\u043e\u0434\u0430\u0442\u0438 \u0447\u0438 \u043f\u043e\u043a\u0440\u0430\u0449\u0438\u0442\u0438 \u043f\u0440\u0438\u043a\u043b\u0430\u0434\u0438", 
    "Article Title Lookup / Link Text": "\u041f\u043e\u0448\u0443\u043a \u0437\u0430\u0433\u043e\u043b\u043e\u0432\u043a\u0443 \u0441\u0442\u0430\u0442\u0442\u0456 / \u0422\u0435\u043a\u0441\u0442 \u043f\u043e\u0441\u0438\u043b\u0430\u043d\u043d\u044f", 
    "Aspect ratio": "\u0421\u043f\u0456\u0432\u0432\u0456\u0434\u043d\u043e\u0448\u0435\u043d\u043d\u044f \u0440\u043e\u0437\u043c\u0456\u0440\u0456\u0432", 
    "Attachments": "\u041f\u0440\u0438\u043a\u0440\u0456\u043f\u043b\u0435\u043d\u043d\u044f", 
    "Autosave disabled.": "\u0410\u0432\u0442\u043e\u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043d\u043d\u044f \u0432\u0438\u043c\u043a\u043d\u0435\u043d\u043e.", 
    "Autosave enabled.": "\u0410\u0432\u0442\u043e\u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043d\u043d\u044f \u0443\u0432\u0456\u043c\u043a\u043d\u0435\u043d\u043e.", 
    "CSS Content": "CSS-\u0432\u043c\u0456\u0441\u0442", 
    "Choose one...": "\u0412\u0438\u0431\u0435\u0440\u0456\u0442\u044c \u043e\u0434\u0438\u043d...", 
    "Close": "\u0417\u0430\u043a\u0440\u0438\u0442\u0438", 
    "Close notification": "\u0417\u0430\u043a\u0440\u0438\u0442\u0438 \u0441\u043f\u043e\u0432\u0456\u0449\u0435\u043d\u043d\u044f", 
    "Close submenu": "\u0417\u0430\u043a\u0440\u0438\u0442\u0438 \u043f\u0456\u0434\u043c\u0435\u043d\u044e", 
    "Create a Redirect": "\u0421\u0442\u0432\u043e\u0440\u0438\u0442\u0438 \u043f\u0435\u0440\u0435\u043d\u0430\u043f\u0440\u0430\u0432\u043b\u0435\u043d\u043d\u044f", 
    "Default": "\u0422\u0438\u043f\u043e\u0432\u043e", 
    "Details": "\u0414\u043e\u043a\u043b\u0430\u0434\u043d\u043e", 
    "Did this page help you?": "\u0426\u044f \u0441\u0442\u043e\u0440\u0456\u043d\u043a\u0430 \u0434\u043e\u043f\u043e\u043c\u043e\u0433\u043b\u0430 \u0432\u0430\u043c?", 
    "Discard the draft": "\u0412\u0438\u0434\u0430\u043b\u0438\u0442\u0438 \u0447\u0435\u0440\u043d\u0435\u0442\u043a\u0443", 
    "Document": "\u0414\u043e\u043a\u0443\u043c\u0435\u043d\u0442", 
    "Draft": "\u0427\u0435\u0440\u043d\u0435\u0442\u043a\u0430", 
    "Embed YouTube Video": "\u0412\u0441\u0442\u0430\u0432\u0438\u0442\u0438 YouTube \u0432\u0456\u0434\u0435\u043e", 
    "Enable autosave.": "\u0423\u0432\u0456\u043c\u043a\u043d\u0443\u0442\u0438 \u0430\u0432\u0442\u043e\u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043d\u043d\u044f.", 
    "Fix incorrect information": "\u0412\u0438\u043f\u0440\u0430\u0432\u0438\u0442\u0438 \u0445\u0438\u0431\u043d\u0456 \u0432\u0456\u0434\u043e\u043c\u043e\u0441\u0442\u0456", 
    "HTML Content": "HTML-\u0432\u043c\u0456\u0441\u0442", 
    "How to read CSS syntax.": "\u042f\u043a \u0447\u0438\u0442\u0430\u0442\u0438 CSS \u0441\u0438\u043d\u0442\u0430\u043a\u0441\u0438\u0441.", 
    "Insert Code Sample Template": "\u0412\u0441\u0442\u0430\u0432\u0438\u0442\u0438 \u0437\u0440\u0430\u0437\u043e\u043a \u043a\u043e\u0434\u0443 \u0448\u0430\u0431\u043b\u043e\u043d\u0443", 
    "Insert Code Sample iFrame": "\u0412\u0441\u0442\u0430\u0432\u0438\u0442\u0438 \u0437\u0440\u0430\u0437\u043e\u043a \u043a\u043e\u0434\u0443 iFrame", 
    "JavaScript Content": "JavaScript-\u0432\u043c\u0456\u0441\u0442", 
    "Launch": "\u0417\u0430\u043f\u0443\u0441\u0442\u0438\u0442\u0438", 
    "Locate a YouTube Video": "\u0417\u043d\u0430\u0439\u0434\u0456\u0442\u044c YouTube \u0432\u0456\u0434\u0435\u043e", 
    "MDN Redirect": "MDN \u043f\u0435\u0440\u0435\u043d\u0430\u043f\u0440\u0430\u0432\u043b\u0435\u043d\u043d\u044f", 
    "Make explanations clearer": "\u0417\u0440\u043e\u0431\u0438\u0442\u0438 \u043f\u043e\u044f\u0441\u043d\u0435\u043d\u043d\u044f \u0437\u0440\u043e\u0437\u0443\u043c\u0456\u043b\u0456\u0448\u0438\u043c", 
    "More about the beta.": "\u0414\u0435\u0442\u0430\u043b\u044c\u043d\u0456\u0448\u0435 \u043f\u0440\u043e \u0431\u0435\u0442\u0430-\u0432\u0435\u0440\u0441\u0456\u0457.", 
    "My search should have led to a different article": "\u041c\u0456\u0439 \u043f\u043e\u0448\u0443\u043a \u043f\u043e\u0432\u0438\u043d\u0435\u043d \u0432\u0435\u0441\u0442\u0438 \u043d\u0430 \u0456\u043d\u0448\u0443 \u0441\u0442\u0430\u0442\u0442\u044e", 
    "Never ask me again": "\u0411\u0456\u043b\u044c\u0448\u0435 \u043d\u0435 \u0437\u0430\u043f\u0438\u0442\u0443\u0432\u0430\u0442\u0438", 
    "New compatibility tables are in beta ": "\u041d\u043e\u0432\u0456 \u0442\u0430\u0431\u043b\u0438\u0446\u0456 \u0441\u0443\u043c\u0456\u0441\u043d\u043e\u0441\u0442\u0456 \u0432 \u0431\u0435\u0442\u0430-\u0432\u0435\u0440\u0441\u0456\u0457 ", 
    "New interest...": "\u041d\u043e\u0432\u0435 \u0437\u0430\u0446\u0456\u043a\u0430\u0432\u043b\u0435\u043d\u043d\u044f...", 
    "New tag...": "\u041d\u043e\u0432\u0430 \u043c\u0456\u0442\u043a\u0430...", 
    "No": "\u041d\u0456", 
    "No Highlight": "\u041d\u0435\u043c\u0430\u0454 \u0432\u0438\u0434\u0456\u043b\u0435\u043d\u043d\u044f", 
    "No attachments available": "\u041d\u0435\u043c\u0430\u0454 \u0434\u043e\u0441\u0442\u0443\u043f\u043d\u0438\u0445 \u043f\u0440\u0438\u043a\u0440\u0456\u043f\u043b\u0435\u043d\u044c", 
    "No selection": "\u041d\u0435 \u0432\u0438\u0431\u0440\u0430\u043d\u043e", 
    "Open": "\u0412\u0456\u0434\u043a\u0440\u0438\u0442\u0438", 
    "Open implementation notes": "\u0412\u0456\u0434\u043a\u0440\u0438\u0442\u0438 \u043f\u0440\u0438\u043c\u0456\u0442\u043a\u0438 \u0440\u0435\u0430\u043b\u0456\u0437\u0430\u0446\u0456\u0457", 
    "Paste YouTube Video URL": "\u0412\u0441\u0442\u0430\u0432\u0438\u0442\u0438 URL \u0432\u0456\u0434\u0435\u043e \u0437 YouTube", 
    "Report an error.": "\u041f\u043e\u0432\u0456\u0434\u043e\u043c\u0438\u0442\u0438 \u043f\u0440\u043e \u043f\u043e\u043c\u0438\u043b\u043a\u0443.", 
    "Reported. Thanks!": "\u041f\u043e\u0432\u0456\u0434\u043e\u043c\u043b\u0435\u043d\u043e. \u0414\u044f\u043a\u0443\u044e!", 
    "Restore the draft content": "\u0412\u0456\u0434\u043d\u043e\u0432\u0438\u0442\u0438 \u0432\u043c\u0456\u0441\u0442 \u0447\u0435\u0440\u043d\u0435\u0442\u043a\u0438", 
    "Return to compatibility table.": "\u041f\u043e\u0432\u0435\u0440\u043d\u0443\u0442\u0438\u0441\u044f \u0434\u043e \u0442\u0430\u0431\u043b\u0438\u0446\u0456 \u0441\u0443\u043c\u0456\u0441\u043d\u043e\u0441\u0442\u0456.", 
    "Sample CSS Content": "\u0417\u0440\u0430\u0437\u043e\u043a CSS-\u0432\u043c\u0456\u0441\u0442\u0443", 
    "Sample Finder": "\u041f\u043e\u0448\u0443\u043a \u043f\u0440\u0438\u043a\u043b\u0430\u0434\u0456\u0432", 
    "Sample HTML Content": "\u041f\u0440\u0438\u043a\u043b\u0430\u0434 HTML-\u0432\u043c\u0456\u0441\u0442\u0443", 
    "Sample JavaScript Content": "\u0417\u0440\u0430\u0437\u043e\u043a JavaScript-\u0432\u043c\u0456\u0441\u0442\u0443", 
    "Save Draft": "\u0417\u0431\u0435\u0440\u0435\u0433\u0442\u0438 \u0447\u0435\u0440\u043d\u0435\u0442\u043a\u0443", 
    "Search Stack Overflow": "\u041f\u043e\u0448\u0443\u043a \u043d\u0430 Stack Overflow", 
    "Sections in Document": "\u0420\u043e\u0437\u0434\u0456\u043b\u0438 \u0432 \u0434\u043e\u043a\u0443\u043c\u0435\u043d\u0442\u0456", 
    "Select a section": "\u0412\u0438\u0431\u0456\u0440 \u0440\u043e\u0437\u0434\u0456\u043b\u0443", 
    "Select an attachment": "\u0412\u0438\u0431\u0440\u0430\u0442\u0438 \u043f\u0440\u0438\u043a\u0440\u0456\u043f\u043b\u0435\u043d\u043d\u044f", 
    "Selected: ": "\u0412\u0438\u0431\u0440\u0430\u043d\u043e: ", 
    "Show old table.": "\u041f\u043e\u043a\u0430\u0437\u0430\u0442\u0438 \u0441\u0442\u0430\u0440\u0443 \u0442\u0430\u0431\u043b\u0438\u0446\u044e.", 
    "Something else": "\u0429\u043e\u0441\u044c \u0456\u043d\u0448\u0435", 
    "Syntax Highlighter": "\u041f\u0456\u0434\u0441\u0432\u0456\u0447\u0443\u0432\u0430\u043d\u043d\u044f \u0441\u0438\u043d\u0442\u0430\u043a\u0441\u0438\u0441\u0443", 
    "Take the survey": "\u0412\u0437\u044f\u0442\u0438 \u0443\u0447\u0430\u0441\u0442\u044c \u0432 \u043e\u043f\u0438\u0442\u0443\u0432\u0430\u043d\u043d\u0456", 
    "Thanks! We'll fix it.": "\u0414\u044f\u043a\u0443\u044e! \u041c\u0438 \u0432\u0438\u043f\u0440\u0430\u0432\u0438\u043c\u043e \u0446\u0435.", 
    "The URL you've entered doesn't appear to be valid": "\u0421\u0445\u043e\u0436\u0435, \u0432\u0438 \u0432\u0432\u0435\u043b\u0438 \u043d\u0435\u0434\u0456\u0439\u0441\u043d\u0443 URL", 
    "Translate it into my language": "\u041f\u0435\u0440\u0435\u043a\u043b\u0430\u0441\u0442\u0438 \u0446\u0435 \u043c\u043e\u0454\u044e \u043c\u043e\u0432\u043e\u044e", 
    "URL": "URL", 
    "Uh oh. What would make it better?": "\u041e\u0445-\u043e\u0445. \u042f\u043a \u0437\u0440\u043e\u0431\u0438\u0442\u0438 \u0446\u0435 \u043a\u0440\u0430\u0449\u0438\u043c?", 
    "What should the sample title be?": "\u042f\u043a\u0438\u043c \u043f\u043e\u0432\u0438\u043d\u0435\u043d \u0431\u0443\u0442\u0438 \u0437\u0430\u0433\u043e\u043b\u043e\u0432\u043e\u043a \u0437\u0440\u0430\u0437\u043a\u0430?", 
    "Yes": "\u0422\u0430\u043a", 
    "You have a draft from:": "\u0423 \u0432\u0430\u0441 \u0454 \u0447\u0435\u0440\u043d\u0435\u0442\u043a\u0430 \u0432\u0456\u0434:", 
    "You must input a valid YouTube video URL.": "\u0412\u0438 \u043f\u043e\u0432\u0438\u043d\u043d\u0456 \u0432\u0432\u0435\u0441\u0442\u0438 \u043f\u0440\u0430\u0432\u0438\u043b\u044c\u043d\u0438\u0439 URL \u0432\u0456\u0434\u0435\u043e \u0437 YouTube.", 
    "Your browser does not support MathML. A CSS fallback has been used instead.": "\u0412\u0430\u0448 \u0431\u0440\u0430\u0443\u0437\u0435\u0440 \u043d\u0435 \u043f\u0456\u0434\u0442\u0440\u0438\u043c\u0443\u0454 MathML. \u0417\u0430\u043c\u0456\u0441\u0442\u044c \u0446\u044c\u043e\u0433\u043e \u0432\u0438\u043a\u043e\u0440\u0438\u0441\u0442\u043e\u0432\u0443\u0454\u0442\u044c\u0441\u044f \u0437\u0430\u043f\u0430\u0441\u043d\u0435 \u0432\u0438\u0440\u0456\u0448\u0435\u043d\u043d\u044f \u043d\u0430 CSS.", 
    "an unknown date": "\u043d\u0435\u0432\u0456\u0434\u043e\u043c\u0430 \u0434\u0430\u0442\u0430", 
    "discarded": "\u0441\u043a\u0430\u0441\u043e\u0432\u0430\u043d\u043e", 
    "published": "\u043e\u043f\u0443\u0431\u043b\u0456\u043a\u043e\u0432\u0430\u043d\u043e", 
    "restored": "\u0432\u0456\u0434\u043d\u043e\u0432\u043b\u0435\u043d\u043e"
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
    "DATETIME_FORMAT": "j E Y \u0440. H:i", 
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
    "DATE_FORMAT": "j E Y \u0440.", 
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
    "DECIMAL_SEPARATOR": ",", 
    "FIRST_DAY_OF_WEEK": "1", 
    "MONTH_DAY_FORMAT": "j F", 
    "NUMBER_GROUPING": "0", 
    "SHORT_DATETIME_FORMAT": "m/d/Y P", 
    "SHORT_DATE_FORMAT": "j M Y", 
    "THOUSAND_SEPARATOR": "\u00a0", 
    "TIME_FORMAT": "H:i", 
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

