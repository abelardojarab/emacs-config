

(function (globals) {

  var django = globals.django || (globals.django = {});

  
  django.pluralidx = function (n) {
    var v=(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2);
    if (typeof(v) == 'boolean') {
      return v ? 1 : 0;
    } else {
      return v;
    }
  };
  

  
  /* gettext library */

  django.catalog = {
    "Add more details": "Wymaga wi\u0119cej szczeg\u00f3\u0142\u00f3w", 
    "Add or improve examples": "Wymaga dodania lub ulepszenia przyk\u0142ad\u00f3w", 
    "Article Title Lookup / Link Text": "Podgl\u0105d tytu\u0142u artyku\u0142u / Tekst odno\u015bnika", 
    "Aspect ratio": "Skala obrazu", 
    "Attachments": "Za\u0142\u0105czniki", 
    "CSS Content": "Zawarto\u015b\u0107 CSS", 
    "Choose one...": "Wybierz jedno...", 
    "Close": "Zamknij", 
    "Close notification": "Zamknij powiadomienie", 
    "Close submenu": "Zamknij podmenu", 
    "Create a Redirect": "Stw\u00f3rz przekierowanie", 
    "Default": "Domy\u015blny", 
    "Details": "Szczeg\u00f3\u0142y", 
    "Did this page help you?": "Czy ta strona jest pomocna?", 
    "Discard the draft": "Odrzu\u0107 szkic", 
    "Document": "Dokument", 
    "Embed YouTube Video": "Wstaw wideo z YouTube", 
    "Fix incorrect information": "Wymaga poprawienia b\u0142\u0119dnych informacji", 
    "HTML Content": "Zawarto\u015b\u0107 HTML", 
    "How to read CSS syntax.": "Jak czyta\u0107 CSS.", 
    "Insert Code Sample Template": "Wstaw szablon przyk\u0142adowego kodu", 
    "Insert Code Sample iFrame": "Wstaw pr\u00f3bk\u0119 kodu iFrame", 
    "JavaScript Content": "Zawarto\u015b\u0107 JavaScript", 
    "Launch": "Uruchom", 
    "Locate a YouTube Video": "Znajd\u017a wideo z YouTube", 
    "MDN Redirect": "Przekierowanie MDN", 
    "Make explanations clearer": "Wymaga uproszczenia informacji", 
    "More about the beta.": "Wi\u0119cej o fazie beta.", 
    "My search should have led to a different article": "Ta strona nie pasuje do mojego wyszukiwania", 
    "Never ask me again": "Nie pytaj ponownie", 
    "New compatibility tables are in beta ": "Nowe tabele kompatybilno\u015bci s\u0105 w fazie beta ", 
    "New interest...": "Nowe zainteresowanie...", 
    "New tag...": "Nowa etykieta...", 
    "No": "Nie", 
    "No Highlight": "Bez pod\u015bwietlenia", 
    "No attachments available": "Brak za\u0142\u0105cznik\u00f3w", 
    "No selection": "Brak wyboru", 
    "Open": "Otw\u00f3rz", 
    "Open implementation notes": "Otw\u00f3rz notatki implementacyjne", 
    "Paste YouTube Video URL": "Wklej URL wideo z YouTube", 
    "Report an error.": "Zg\u0142o\u015b b\u0142\u0105d.", 
    "Reported. Thanks!": "Zg\u0142oszono. Dzi\u0119kujemy!", 
    "Restore the draft content": "Przywr\u00f3\u0107 zawarto\u015b\u0107 szkicu", 
    "Return to compatibility table.": "Wr\u00f3\u0107 do tabeli kompatybilno\u015bci.", 
    "Sample CSS Content": "Przyk\u0142adowa zawarto\u015b\u0107 CSS", 
    "Sample Finder": "Wyszukiwarka przyk\u0142ad\u00f3w", 
    "Sample HTML Content": "Przyk\u0142adowa zawarto\u015b\u0107 HTML", 
    "Sample JavaScript Content": "Przyk\u0142adowa zawarto\u015b\u0107 JavaScript", 
    "Search Stack Overflow": "Przeszukaj Stack Overflow", 
    "Sections in Document": "Sekcje dokumentu", 
    "Select a section": "Wybierz sekcj\u0119", 
    "Select an attachment": "Wybierz za\u0142\u0105cznik", 
    "Selected: ": "Wybrane: ", 
    "Show old table.": "Poka\u017c star\u0105 tabel\u0119.", 
    "Something else": "Co\u015b innego", 
    "Syntax Highlighter": "Pod\u015bwietl sk\u0142adni\u0119", 
    "Take the survey": "Wype\u0142nij ankiet\u0119", 
    "Thanks! We'll fix it.": "Dzi\u0119kujemy! Popracujemy nad tym.", 
    "The URL you've entered doesn't appear to be valid": "Podany URL nie wygl\u0105da na poprawny", 
    "Translate it into my language": "Wymaga t\u0142umaczenia na m\u00f3j j\u0119zyk", 
    "URL": "URL", 
    "Uh oh. What would make it better?": "Ups. Jak mo\u017cemy j\u0105 ulepszy\u0107?", 
    "What should the sample title be?": "Jaki ma by\u0107 tytu\u0142 przyk\u0142adu?", 
    "Yes": "Tak", 
    "You have a draft from:": "Masz szkic od:", 
    "You must input a valid YouTube video URL.": "Podaj prawid\u0142owy URL wideo z YouTube.", 
    "Your browser does not support MathML. A CSS fallback has been used instead.": "Twoja przegl\u0105darka nie obs\u0142uguje MathML. Zamiast tego korzystasz z alternatywnej wersji CSS.", 
    "an unknown date": "nieznana data"
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
    "DATETIME_FORMAT": "j E Y H:i", 
    "DATETIME_INPUT_FORMATS": [
      "%d.%m.%Y %H:%M:%S", 
      "%d.%m.%Y %H:%M:%S.%f", 
      "%d.%m.%Y %H:%M", 
      "%d.%m.%Y", 
      "%Y-%m-%d %H:%M:%S", 
      "%Y-%m-%d %H:%M:%S.%f", 
      "%Y-%m-%d %H:%M", 
      "%Y-%m-%d"
    ], 
    "DATE_FORMAT": "j E Y", 
    "DATE_INPUT_FORMATS": [
      "%d.%m.%Y", 
      "%d.%m.%y", 
      "%y-%m-%d", 
      "%Y-%m-%d"
    ], 
    "DECIMAL_SEPARATOR": ",", 
    "FIRST_DAY_OF_WEEK": "1", 
    "MONTH_DAY_FORMAT": "j F", 
    "NUMBER_GROUPING": "3", 
    "SHORT_DATETIME_FORMAT": "d-m-Y  H:i", 
    "SHORT_DATE_FORMAT": "d-m-Y", 
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

