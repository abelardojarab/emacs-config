

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
    "Add more details": "Voeg meer details toe", 
    "Add or improve examples": "Voeg voorbeelden toe of verbeter ze", 
    "Article Title Lookup / Link Text": "Zoeken naar artikeltitel / koppelingstekst", 
    "Aspect ratio": "Hoogte-breedteverhouding", 
    "Attachments": "Bijlagen", 
    "Autosave disabled.": "Automatisch opslaan uitgeschakeld.", 
    "Autosave enabled.": "Automatisch opslaan ingeschakeld.", 
    "CSS Content": "CSS-inhoud", 
    "Choose one...": "Kies er een\u2026", 
    "Close": "Sluiten", 
    "Close notification": "Melding sluiten", 
    "Close submenu": "Submenu sluiten", 
    "Create a Redirect": "Een omleiding maken", 
    "Default": "Standaard", 
    "Details": "Details", 
    "Did this page help you?": "Heeft deze pagina geholpen?", 
    "Discard the draft": "Het concept verwijderen", 
    "Document": "Document", 
    "Draft": "Concept", 
    "Embed YouTube Video": "YouTube-video inbedden", 
    "Enable autosave.": "Automatisch opslaan inschakelen.", 
    "Fix incorrect information": "Maak onjuiste info in orde", 
    "HTML Content": "HTML-inhoud", 
    "How to read CSS syntax.": "CSS-syntaxis lezen.", 
    "Insert Code Sample Template": "Codevoorbeeldsjabloon invoegen", 
    "Insert Code Sample iFrame": "Codevoorbeeld-iFrame invoegen", 
    "JavaScript Content": "JavaScript-inhoud", 
    "Launch": "Starten", 
    "Locate a YouTube Video": "Een YouTube-video zoeken", 
    "MDN Redirect": "MDN-omleiding", 
    "Make explanations clearer": "Maak instructies duidelijker", 
    "More about the beta.": "Meer over de b\u00e8ta.", 
    "My search should have led to a different article": "Mijn zoekopdracht zou naar een ander artikel moeten leiden", 
    "Never ask me again": "Nooit meer vragen", 
    "New compatibility tables are in beta ": "Nieuwe compatibiliteitstabellen zijn in b\u00e8ta ", 
    "New interest...": "Nieuwe interesse\u2026", 
    "New tag...": "Nieuw label\u2026", 
    "No": "Nee", 
    "No Highlight": "Geen markering", 
    "No attachments available": "Geen bijlagen beschikbaar", 
    "No selection": "Geen selectie", 
    "Open": "Openen", 
    "Open implementation notes": "Implementatieopmerkingen openen", 
    "Paste YouTube Video URL": "YouTube-video-URL plakken", 
    "Report an error.": "Een fout melden", 
    "Reported. Thanks!": "Gemeld. Bedankt!", 
    "Restore the draft content": "De conceptinhoud herstellen", 
    "Return to compatibility table.": "Terug naar compatibiliteitstabel", 
    "Sample CSS Content": "Voorbeeld van CSS-inhoud", 
    "Sample Finder": "Voorbeeldzoeker", 
    "Sample HTML Content": "Voorbeeld van HTML-inhoud", 
    "Sample JavaScript Content": "Voorbeeld van JavaScript-inhoud", 
    "Save Draft": "Concept opslaan", 
    "Search Stack Overflow": "Zoeken bij Stack Overflow", 
    "Sections in Document": "Secties in document", 
    "Select a section": "Selecteer een sectie", 
    "Select an attachment": "Selecteer een bijlage", 
    "Selected: ": "Geselecteerd: ", 
    "Show old table.": "Oude tabel tonen", 
    "Something else": "Iets anders", 
    "Syntax Highlighter": "Syntaxismarkeerder", 
    "Take the survey": "De enqu\u00eate starten", 
    "Thanks! We'll fix it.": "Bedankt! We maken het in orde.", 
    "The URL you've entered doesn't appear to be valid": "De URL die u hebt ingevoerd lijkt niet geldig te zijn", 
    "Translate it into my language": "Vertaal hem naar mijn taal", 
    "URL": "URL", 
    "Uh oh. What would make it better?": "O, o. Wat zou het beter kunnen maken?", 
    "What should the sample title be?": "Wat zou de voorbeeldtitel moeten zijn?", 
    "Yes": "Ja", 
    "You have a draft from:": "U hebt een concept van:", 
    "You must input a valid YouTube video URL.": "U moet een geldige YouTube-video-URL opgeven.", 
    "Your browser does not support MathML. A CSS fallback has been used instead.": "Uw browser ondersteunt geen MathML. Daarom is een CSS-terugvalversie gebruikt.", 
    "an unknown date": "een onbekende datum", 
    "discarded": "verworpen", 
    "published": "gepubliceerd", 
    "restored": "hersteld"
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
    "DATETIME_FORMAT": "j F Y H:i", 
    "DATETIME_INPUT_FORMATS": [
      "%d-%m-%Y %H:%M:%S", 
      "%d-%m-%y %H:%M:%S", 
      "%Y-%m-%d %H:%M:%S", 
      "%d/%m/%Y %H:%M:%S", 
      "%d/%m/%y %H:%M:%S", 
      "%Y/%m/%d %H:%M:%S", 
      "%d-%m-%Y %H:%M:%S.%f", 
      "%d-%m-%y %H:%M:%S.%f", 
      "%Y-%m-%d %H:%M:%S.%f", 
      "%d/%m/%Y %H:%M:%S.%f", 
      "%d/%m/%y %H:%M:%S.%f", 
      "%Y/%m/%d %H:%M:%S.%f", 
      "%d-%m-%Y %H.%M:%S", 
      "%d-%m-%y %H.%M:%S", 
      "%d/%m/%Y %H.%M:%S", 
      "%d/%m/%y %H.%M:%S", 
      "%d-%m-%Y %H.%M:%S.%f", 
      "%d-%m-%y %H.%M:%S.%f", 
      "%d/%m/%Y %H.%M:%S.%f", 
      "%d/%m/%y %H.%M:%S.%f", 
      "%d-%m-%Y %H:%M", 
      "%d-%m-%y %H:%M", 
      "%Y-%m-%d %H:%M", 
      "%d/%m/%Y %H:%M", 
      "%d/%m/%y %H:%M", 
      "%Y/%m/%d %H:%M", 
      "%d-%m-%Y %H.%M", 
      "%d-%m-%y %H.%M", 
      "%d/%m/%Y %H.%M", 
      "%d/%m/%y %H.%M", 
      "%d-%m-%Y", 
      "%d-%m-%y", 
      "%Y-%m-%d", 
      "%d/%m/%Y", 
      "%d/%m/%y", 
      "%Y/%m/%d"
    ], 
    "DATE_FORMAT": "j F Y", 
    "DATE_INPUT_FORMATS": [
      "%d-%m-%Y", 
      "%d-%m-%y", 
      "%d/%m/%Y", 
      "%d/%m/%y", 
      "%Y-%m-%d"
    ], 
    "DECIMAL_SEPARATOR": ",", 
    "FIRST_DAY_OF_WEEK": "1", 
    "MONTH_DAY_FORMAT": "j F", 
    "NUMBER_GROUPING": "3", 
    "SHORT_DATETIME_FORMAT": "j-n-Y H:i", 
    "SHORT_DATE_FORMAT": "j-n-Y", 
    "THOUSAND_SEPARATOR": ".", 
    "TIME_FORMAT": "H:i", 
    "TIME_INPUT_FORMATS": [
      "%H:%M:%S", 
      "%H:%M:%S.%f", 
      "%H.%M:%S", 
      "%H.%M:%S.%f", 
      "%H.%M", 
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

