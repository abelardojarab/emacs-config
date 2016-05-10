

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
    "Add more details": "Adjon meg tov\u00e1bbi r\u00e9szleteket", 
    "Add or improve examples": "P\u00e9ld\u00e1k hozz\u00e1ad\u00e1sa vagy t\u00f6k\u00e9letes\u00edt\u00e9se", 
    "Article Title Lookup / Link Text": "Cikkc\u00edm-keres\u00e9s / hivatkoz\u00e1ssz\u00f6veg", 
    "Aspect ratio": "M\u00e9retar\u00e1ny", 
    "Attachments": "Mell\u00e9kletek", 
    "CSS Content": "CSS tartalom", 
    "Choose one...": "V\u00e1lasszon egyet\u2026", 
    "Close": "Bez\u00e1r\u00e1s", 
    "Close notification": "\u00c9rtes\u00edt\u00e9s bez\u00e1r\u00e1sa", 
    "Close submenu": "Almen\u00fc bez\u00e1r\u00e1sa", 
    "Create a Redirect": "\u00c1tir\u00e1ny\u00edt\u00e1s l\u00e9trehoz\u00e1sa", 
    "Default": "Alap\u00e9rtelmezett", 
    "Details": "R\u00e9szletek", 
    "Did this page help you?": "Seg\u00edtett ez az oldal?", 
    "Discard the draft": "Piszkozat elvet\u00e9se", 
    "Document": "Dokumentum", 
    "Embed YouTube Video": "YouTube vide\u00f3 be\u00e1gyaz\u00e1sa", 
    "Fix incorrect information": "Jav\u00edtsa a hib\u00e1s inform\u00e1ci\u00f3kat", 
    "HTML Content": "HTML tartalom", 
    "How to read CSS syntax.": "Hogyan olvasand\u00f3 a CSS szintaxis.", 
    "Insert Code Sample Template": "Mintak\u00f3d sablon besz\u00far\u00e1sa", 
    "Insert Code Sample iFrame": "P\u00e9ldak\u00f3d iFrame beilleszt\u00e9se", 
    "JavaScript Content": "JavaScript tartalom", 
    "Launch": "Ind\u00edt\u00e1s", 
    "Locate a YouTube Video": "YouTube vide\u00f3 keres\u00e9se", 
    "MDN Redirect": "MDN \u00e1tir\u00e1ny\u00edt\u00e1s", 
    "Make explanations clearer": "Adjon r\u00e9szletesebb magyar\u00e1zatot", 
    "More about the beta.": "Tov\u00e1bbi inform\u00e1ci\u00f3k a b\u00e9ta verzi\u00f3r\u00f3l.", 
    "My search should have led to a different article": "A keres\u00e9s m\u00e1s cikkre kellett volna vezessen", 
    "Never ask me again": "Ez a k\u00e9rd\u00e9s a j\u00f6v\u0151ben ne jelenjen meg", 
    "New compatibility tables are in beta ": "\u00daj kompatibilit\u00e1si t\u00e1bl\u00e1zatok a b\u00e9ta verzi\u00f3ban ", 
    "New interest...": "\u00daj \u00e9rdekl\u0151d\u00e9si k\u00f6r\u2026", 
    "New tag...": "\u00daj c\u00edmke\u2026", 
    "No": "Nem", 
    "No Highlight": "Nincs kiemel\u00e9s", 
    "No attachments available": "Nem el\u00e9rhet\u0151 el mell\u00e9klet", 
    "No selection": "Nincs kiv\u00e1lasztva", 
    "Open": "Megnyit\u00e1s", 
    "Open implementation notes": "Megval\u00f3s\u00edt\u00e1si megjegyz\u00e9sek megnyit\u00e1sa", 
    "Paste YouTube Video URL": "YouTube vide\u00f3 URL beilleszt\u00e9se", 
    "Report an error.": "Hiba bejelent\u00e9se.", 
    "Reported. Thanks!": "Bejelentve. K\u00f6sz\u00f6nj\u00fck!", 
    "Restore the draft content": "Piszkozat tartalm\u00e1nak helyre\u00e1ll\u00edt\u00e1sa", 
    "Return to compatibility table.": "Visszat\u00e9r\u00e9s a kompatibilit\u00e1si t\u00e1bl\u00e1zathoz.", 
    "Sample CSS Content": "Minta CSS tartalom", 
    "Sample Finder": "Mintakeres\u0151", 
    "Sample HTML Content": "Minta HTML tartalom", 
    "Sample JavaScript Content": "Minta JavaScript tartalom", 
    "Search Stack Overflow": "Keres\u00e9s a Stack Overflow oldalon", 
    "Sections in Document": "Dokumentum szakaszai", 
    "Select a section": "Szakasz kijel\u00f6l\u00e9se", 
    "Select an attachment": "Mell\u00e9klet kiv\u00e1laszt\u00e1sa", 
    "Selected: ": "Kiv\u00e1lasztva: ", 
    "Show old table.": "R\u00e9gi t\u00e1bl\u00e1zat megjelen\u00edt\u00e9se.", 
    "Something else": "Valami m\u00e1s", 
    "Syntax Highlighter": "Szintaxis kiemel\u0151", 
    "Take the survey": "K\u00e9rd\u0151\u00edv kit\u00ed\u00f6lt\u00e9se", 
    "Thanks! We'll fix it.": "K\u00f6sz\u00f6nj\u00fck. Jav\u00edtjuk.", 
    "The URL you've entered doesn't appear to be valid": "A megadott URL val\u00f3sz\u00edn\u0171leg \u00e9rv\u00e9nytelen", 
    "Translate it into my language": "Ford\u00edt\u00e1s a saj\u00e1t nyelv\u00e9re", 
    "URL": "URL", 
    "Uh oh. What would make it better?": "Ajjaj. Mi tehetn\u00e9 jobb\u00e1?", 
    "What should the sample title be?": "Mi legyen a minta c\u00edme?", 
    "Yes": "Igen", 
    "You have a draft from:": "L\u00e9tezik egy piszkozata innen:", 
    "You must input a valid YouTube video URL.": "\u00c9rv\u00e9nyes YouTube vide\u00f3 URL-t kell megadnia.", 
    "Your browser does not support MathML. A CSS fallback has been used instead.": "B\u00f6ng\u00e9sz\u0151je nem t\u00e1mogatja a MathML-t. Egy CSS tartal\u00e9kmegold\u00e1s lett haszn\u00e1lva helyette.", 
    "an unknown date": "ismeretlen d\u00e1tum"
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
    "DATETIME_FORMAT": "Y. F j. G.i", 
    "DATETIME_INPUT_FORMATS": [
      "%Y.%m.%d. %H.%M.%S", 
      "%Y.%m.%d. %H.%M.%S.%f", 
      "%Y.%m.%d. %H.%M", 
      "%Y.%m.%d.", 
      "%Y-%m-%d %H:%M:%S", 
      "%Y-%m-%d %H:%M:%S.%f", 
      "%Y-%m-%d %H:%M", 
      "%Y-%m-%d"
    ], 
    "DATE_FORMAT": "Y. F j.", 
    "DATE_INPUT_FORMATS": [
      "%Y.%m.%d.", 
      "%Y-%m-%d"
    ], 
    "DECIMAL_SEPARATOR": ",", 
    "FIRST_DAY_OF_WEEK": "1", 
    "MONTH_DAY_FORMAT": "F j.", 
    "NUMBER_GROUPING": "3", 
    "SHORT_DATETIME_FORMAT": "Y.m.d. G.i", 
    "SHORT_DATE_FORMAT": "Y.m.d.", 
    "THOUSAND_SEPARATOR": "\u00a0", 
    "TIME_FORMAT": "G.i", 
    "TIME_INPUT_FORMATS": [
      "%H.%M.%S", 
      "%H.%M", 
      "%H:%M:%S", 
      "%H:%M:%S.%f", 
      "%H:%M"
    ], 
    "YEAR_MONTH_FORMAT": "Y. F"
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

