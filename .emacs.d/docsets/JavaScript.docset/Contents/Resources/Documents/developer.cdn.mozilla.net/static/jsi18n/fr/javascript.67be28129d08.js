

(function (globals) {

  var django = globals.django || (globals.django = {});

  
  django.pluralidx = function (n) {
    var v=(n > 1);
    if (typeof(v) == 'boolean') {
      return v ? 1 : 0;
    } else {
      return v;
    }
  };
  

  
  /* gettext library */

  django.catalog = {
    "Add more details": "Ajouter plus de d\u00e9tails", 
    "Add or improve examples": "Ajouter ou am\u00e9liorer les exemples", 
    "Article Title Lookup / Link Text": "Recherche d'article par titre / texte du lien", 
    "Aspect ratio": "Facteur de forme", 
    "Attachments": "Pi\u00e8ces jointes", 
    "Autosave disabled.": "Sauvegarde automatique d\u00e9sactiv\u00e9e.", 
    "Autosave enabled.": "Sauvegarde automatique activ\u00e9e.", 
    "CSS Content": "Contenu CSS", 
    "Choose one...": "Choisissez une raison\u2026", 
    "Close": "Fermer", 
    "Close notification": "Fermer la notification", 
    "Close submenu": "Fermer le sous-menu", 
    "Create a Redirect": "Cr\u00e9er une redirection", 
    "Default": "Par d\u00e9faut", 
    "Details": "D\u00e9tails", 
    "Did this page help you?": "Cette page vous a-t-elle \u00e9t\u00e9 utile ?", 
    "Discard the draft": "Supprimer le brouillon", 
    "Document": "Document", 
    "Draft": "Brouillon", 
    "Embed YouTube Video": "Int\u00e9grer une vid\u00e9o YouTube", 
    "Enable autosave.": "Activer l'enregistrement automatique.", 
    "Fix incorrect information": "Corriger des informations incorrectes", 
    "HTML Content": "Contenu HTML", 
    "How to read CSS syntax.": "Comment lire la syntaxe CSS.", 
    "Insert Code Sample Template": "Ins\u00e9rer un mod\u00e8le d'exemple de code", 
    "Insert Code Sample iFrame": "Ins\u00e9rer une iFrame pour une exemple de code", 
    "JavaScript Content": "Contenu JavaScript", 
    "Launch": "Lancer", 
    "Locate a YouTube Video": "D\u00e9terminer l'emplacement de la vid\u00e9o YouTube", 
    "MDN Redirect": "Redirection MDN", 
    "Make explanations clearer": "Clarifier les explications", 
    "More about the beta.": "En savoir plus sur la beta.", 
    "My search should have led to a different article": "Ma recherche aurait d\u00fb aboutir sur un autre article", 
    "Never ask me again": "Ne plus me demander", 
    "New compatibility tables are in beta ": "Les nouveaux tableaux de compatibilit\u00e9 sont disponibles en beta ", 
    "New interest...": "Nouvel int\u00e9r\u00eat\u2026", 
    "New tag...": "Nouvelle \u00e9tiquette\u2026", 
    "No": "Non", 
    "No Highlight": "Pas de coloration", 
    "No attachments available": "Aucune pi\u00e8ce jointe disponible", 
    "No selection": "Aucune s\u00e9lection", 
    "Open": "Ouvrir", 
    "Open implementation notes": "Ouvrir les notes d'impl\u00e9mentation", 
    "Paste YouTube Video URL": "Copier l'URL d'une vid\u00e9o YouTube", 
    "Report an error.": "Rapporter une erreur.", 
    "Reported. Thanks!": "C'est rapport\u00e9. Merci !", 
    "Restore the draft content": "Appliquer le contenu du brouillon", 
    "Return to compatibility table.": "Revenir au tableau de compatibilit\u00e9.", 
    "Sample CSS Content": "Contenu CSS de l'exemple", 
    "Sample Finder": "Explorateur d'exemples", 
    "Sample HTML Content": "Contenu HTML de l'exemple", 
    "Sample JavaScript Content": "Contenu JavaScript de l'exemple", 
    "Save Draft": "Enregistrer le brouillon", 
    "Search Stack Overflow": "Rechercher sur Stack Overflow", 
    "Sections in Document": "Sections du document", 
    "Select a section": "S\u00e9lectionner une section", 
    "Select an attachment": "S\u00e9lectionner une pi\u00e8ce jointe", 
    "Selected: ": "S\u00e9lectionn\u00e9 : ", 
    "Show old table.": "Afficher l'ancien tableau.", 
    "Something else": "Autre chose", 
    "Syntax Highlighter": "Coloration syntaxique", 
    "Take the survey": "R\u00e9pondre au sondage", 
    "Thanks! We'll fix it.": "Merci ! Nous corrigerons \u00e7a.", 
    "The URL you've entered doesn't appear to be valid": "L'URL saisie ne semble pas \u00eatre valide", 
    "Translate it into my language": "Traduire dans ma langue", 
    "URL": "URL", 
    "Uh oh. What would make it better?": "Mince. Qu'est-ce qui pourrait am\u00e9liorer cela ?", 
    "What should the sample title be?": "Quel doit \u00eatre le titre de cet exemple ?", 
    "Yes": "Oui", 
    "You have a draft from:": "Vous avez un brouillon enregistr\u00e9 depuis :", 
    "You must input a valid YouTube video URL.": "Vous devez saisir une URL valide pour une vid\u00e9o YouTube.", 
    "Your browser does not support MathML. A CSS fallback has been used instead.": "Votre navigateur ne supporte pas MathML. Un contenu de remplacement avec CSS a \u00e9t\u00e9 utilis\u00e9.", 
    "an unknown date": "une date inconnue", 
    "discarded": "supprim\u00e9", 
    "published": "publi\u00e9", 
    "restored": "restaur\u00e9"
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
      "%d/%m/%Y %H:%M:%S", 
      "%d/%m/%Y %H:%M:%S.%f", 
      "%d/%m/%Y %H:%M", 
      "%d/%m/%Y", 
      "%d.%m.%Y %H:%M:%S", 
      "%d.%m.%Y %H:%M:%S.%f", 
      "%d.%m.%Y %H:%M", 
      "%d.%m.%Y", 
      "%Y-%m-%d %H:%M:%S", 
      "%Y-%m-%d %H:%M:%S.%f", 
      "%Y-%m-%d %H:%M", 
      "%Y-%m-%d"
    ], 
    "DATE_FORMAT": "j F Y", 
    "DATE_INPUT_FORMATS": [
      "%d/%m/%Y", 
      "%d/%m/%y", 
      "%d.%m.%Y", 
      "%d.%m.%y", 
      "%Y-%m-%d"
    ], 
    "DECIMAL_SEPARATOR": ",", 
    "FIRST_DAY_OF_WEEK": "1", 
    "MONTH_DAY_FORMAT": "j F", 
    "NUMBER_GROUPING": "3", 
    "SHORT_DATETIME_FORMAT": "j N Y H:i", 
    "SHORT_DATE_FORMAT": "j N Y", 
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

