

(function (globals) {

  var django = globals.django || (globals.django = {});

  
  django.pluralidx = function (n) {
    var v=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;
    if (typeof(v) == 'boolean') {
      return v ? 1 : 0;
    } else {
      return v;
    }
  };
  

  
  /* gettext library */

  django.catalog = {
    "Add more details": "P\u0159idejte v\u00edce detail\u016f", 
    "Add or improve examples": "P\u0159idejte nebo vylep\u0161ete p\u0159\u00edklady", 
    "Aspect ratio": "Pom\u011br stran", 
    "Attachments": "P\u0159ilohy", 
    "CSS Content": "CSS obsah", 
    "Choose one...": "Vyberte jednu z mo\u017enost\u00ed...", 
    "Close": "Zav\u0159\u00edt", 
    "Close notification": "Zav\u0159\u00edt ozn\u00e1men\u00ed", 
    "Close submenu": "Zav\u0159\u00edt podmenu", 
    "Create a Redirect": "Vytvo\u0159it p\u0159esm\u011brov\u00e1n\u00ed", 
    "Default": "V\u00fdchoz\u00ed", 
    "Details": "Detaily", 
    "Did this page help you?": "Byla tato str\u00e1nka u\u017eite\u010dn\u00e1?", 
    "Discard the draft": "Zahodit koncept", 
    "Document": "Dokument", 
    "Embed YouTube Video": "Vlo\u017eit YouTube video", 
    "Fix incorrect information": "Opravte nespr\u00e1vn\u00e9 informace", 
    "HTML Content": "HTML obsah", 
    "How to read CSS syntax.": "Jak \u010d\u00edst CSS syntaxi.", 
    "Insert Code Sample Template": "Vlo\u017eit \u0161ablonu uk\u00e1zkov\u00e9ho k\u00f3du", 
    "Insert Code Sample iFrame": "Vlo\u017eit iFrame s uk\u00e1zkou k\u00f3du", 
    "JavaScript Content": "JavaScriptov\u00fd obsah", 
    "Launch": "Spustit", 
    "Locate a YouTube Video": "Naj\u00edt Youtube video", 
    "MDN Redirect": "MDN p\u0159esm\u011brov\u00e1n\u00ed", 
    "Make explanations clearer": "Ud\u011blat vysv\u011btlen\u00ed jasn\u011bj\u0161\u00ed", 
    "More about the beta.": "V\u00edce o t\u00e9to bet\u011b.", 
    "My search should have led to a different article": "M\u00e9 vyhled\u00e1n\u00ed by m\u011blo v\u00e9st na jin\u00fd \u010dl\u00e1nek", 
    "Never ask me again": "Znovu se neptat", 
    "New compatibility tables are in beta ": "Nov\u00e9 tabulky kompatibility jsou aktu\u00e1ln\u011b v bet\u011b ", 
    "New interest...": "Nov\u00fd z\u00e1jem...", 
    "New tag...": "Nov\u00fd \u0161t\u00edtek...", 
    "No": "Ne", 
    "No Highlight": "\u017d\u00e1dn\u00e9 zv\u00fdrazn\u011bn\u00ed", 
    "No attachments available": "\u017d\u00e1dn\u00e9 p\u0159\u00edlohy nejsou dostupn\u00e9", 
    "No selection": "Nic nevybr\u00e1no", 
    "Open": "Otev\u0159\u00edt", 
    "Paste YouTube Video URL": "Vlo\u017ete URL adresu YouTube videa", 
    "Report an error.": "Nahl\u00e1sit chybu.", 
    "Reported. Thanks!": "Nahl\u00e1\u0161eno. D\u011bkujeme!", 
    "Restore the draft content": "Obnovit obsah konceptu", 
    "Return to compatibility table.": "Zp\u011bt na tabulku kompatibility.", 
    "Sample CSS Content": "Uk\u00e1zkov\u00fd CSS obsah", 
    "Sample HTML Content": "Uk\u00e1zkov\u00fd HTML obsah", 
    "Sample JavaScript Content": "Uk\u00e1zkov\u00fd JavaScriptov\u00fd obsah", 
    "Search Stack Overflow": "Prohledat Stack Overflow", 
    "Sections in Document": "\u010c\u00e1sti dokumentu", 
    "Select a section": "Vybrat \u010d\u00e1st", 
    "Select an attachment": "Vybrat p\u0159\u00edlohu", 
    "Selected: ": "Vybr\u00e1no: ", 
    "Show old table.": "Zobrazit starou tabulku.", 
    "Something else": "N\u011bco jin\u00e9ho", 
    "Syntax Highlighter": "Zv\u00fdraz\u0148ova\u010d syntaxe", 
    "Take the survey": "Z\u00fa\u010dastnit se pr\u016fzkumu", 
    "Thanks! We'll fix it.": "D\u011bkujeme! Zkus\u00edme to opravit.", 
    "The URL you've entered doesn't appear to be valid": "Zadan\u00e1 URL nen\u00ed validn\u00ed", 
    "Translate it into my language": "P\u0159elo\u017ete str\u00e1nku do m\u00e9ho jazyka", 
    "URL": "URL", 
    "Uh oh. What would make it better?": "To n\u00e1s mrz\u00ed. Co m\u016f\u017eeme ud\u011blat, abychom to napravili?", 
    "What should the sample title be?": "Jak\u00fd m\u00e1 b\u00fdt uk\u00e1zkov\u00fd nadpis?", 
    "Yes": "Ano", 
    "You have a draft from:": "M\u00e1te koncept od:", 
    "You must input a valid YouTube video URL.": "Mus\u00edte vlo\u017eit platnou URL YouTube videa.", 
    "Your browser does not support MathML. A CSS fallback has been used instead.": "V\u00e1\u0161 prohl\u00ed\u017ee\u010d nepodporuje MathML, proto byl nam\u00edsto n\u011bj pou\u017eit CSS fallback.", 
    "an unknown date": "nezn\u00e1m\u00fd datum"
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
    "DATETIME_FORMAT": "j. E Y G:i", 
    "DATETIME_INPUT_FORMATS": [
      "%d.%m.%Y %H:%M:%S", 
      "%d.%m.%Y %H:%M:%S.%f", 
      "%d.%m.%Y %H.%M", 
      "%d.%m.%Y %H:%M", 
      "%d.%m.%Y", 
      "%d. %m. %Y %H:%M:%S", 
      "%d. %m. %Y %H:%M:%S.%f", 
      "%d. %m. %Y %H.%M", 
      "%d. %m. %Y %H:%M", 
      "%d. %m. %Y", 
      "%Y-%m-%d %H.%M", 
      "%Y-%m-%d %H:%M:%S", 
      "%Y-%m-%d %H:%M:%S.%f", 
      "%Y-%m-%d %H:%M", 
      "%Y-%m-%d"
    ], 
    "DATE_FORMAT": "j. E Y", 
    "DATE_INPUT_FORMATS": [
      "%d.%m.%Y", 
      "%d.%m.%y", 
      "%d. %m. %Y", 
      "%d. %m. %y", 
      "%Y-%m-%d"
    ], 
    "DECIMAL_SEPARATOR": ",", 
    "FIRST_DAY_OF_WEEK": "1", 
    "MONTH_DAY_FORMAT": "j. F", 
    "NUMBER_GROUPING": "3", 
    "SHORT_DATETIME_FORMAT": "d.m.Y G:i", 
    "SHORT_DATE_FORMAT": "d.m.Y", 
    "THOUSAND_SEPARATOR": "\u00a0", 
    "TIME_FORMAT": "G:i", 
    "TIME_INPUT_FORMATS": [
      "%H:%M:%S", 
      "%H.%M", 
      "%H:%M", 
      "%H:%M:%S.%f"
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

