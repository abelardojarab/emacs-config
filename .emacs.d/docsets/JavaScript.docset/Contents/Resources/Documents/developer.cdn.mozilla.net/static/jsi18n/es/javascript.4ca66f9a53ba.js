

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
    "Add more details": "A\u00f1adir m\u00e1s detalles", 
    "Add or improve examples": "A\u00f1adir o mejorar los ejemplos", 
    "Article Title Lookup / Link Text": "B\u00fasqueda del t\u00edtulo del art\u00edculo / texto del v\u00ednculo", 
    "Aspect ratio": "Relaci\u00f3n de aspecto", 
    "Attachments": "Archivos adjuntos", 
    "Autosave disabled.": "Autoguardado desactivado.", 
    "Autosave enabled.": "Autoguardado activado.", 
    "CSS Content": "Contenido CSS", 
    "Choose one...": "Elige uno...", 
    "Close": "Cerrar", 
    "Close notification": "Cerrar notificaci\u00f3n", 
    "Close submenu": "Cerrar submen\u00fa", 
    "Create a Redirect": "Crear una Redirecci\u00f3n", 
    "Default": "Predeterminada", 
    "Details": "Detalles", 
    "Did this page help you?": "\u00bfTe result\u00f3 \u00fatil esta p\u00e1gina?", 
    "Discard the draft": "Descartar el borrador", 
    "Document": "Documento", 
    "Draft": "Borrador", 
    "Embed YouTube Video": "Integrar video de YouTube", 
    "Enable autosave.": "Activar autoguardado.", 
    "Fix incorrect information": "Corregir informaci\u00f3n incorrecta", 
    "HTML Content": "Contenido HTML", 
    "How to read CSS syntax.": "C\u00f3mo leer la sintaxis CSS.", 
    "Insert Code Sample Template": "Insertar plantilla de muestra de c\u00f3digo", 
    "Insert Code Sample iFrame": "Insertar una muestra de c\u00f3digo iFrame", 
    "JavaScript Content": "Contenido JavaScript", 
    "Launch": "Lanzar", 
    "Locate a YouTube Video": "Ubicar un video de YouTube", 
    "MDN Redirect": "Redirecci\u00f3n a MDN", 
    "Make explanations clearer": "Hacer comentarios m\u00e1s completos", 
    "More about the beta.": "Saber m\u00e1s sobre la versi\u00f3n beta.", 
    "My search should have led to a different article": "Mi b\u00fasqueda deber\u00eda haberme dirigido a un art\u00edculo diferente", 
    "Never ask me again": "No volver a preguntar", 
    "New compatibility tables are in beta ": "Las nuevas tablas de compatibilidad est\u00e1n en beta ", 
    "New interest...": "Nuevo inter\u00e9s...", 
    "New tag...": "Nueva etiqueta...", 
    "No": "No", 
    "No Highlight": "No resaltar", 
    "No attachments available": "No hay archivos adjuntos disponibles", 
    "No selection": "No se ha seleccionado nada", 
    "Open": "Abrir", 
    "Open implementation notes": "Abrir notas de implementaci\u00f3n", 
    "Paste YouTube Video URL": "Pegar la URL de un video de YouTube", 
    "Report an error.": "Informar de un error.", 
    "Reported. Thanks!": "Enviado. \u00a1Gracias!", 
    "Restore the draft content": "Restaurar el contenido del borrador", 
    "Return to compatibility table.": "Volver a la tabla de compatibilidad.", 
    "Sample CSS Content": "Muestra de contenido CSS", 
    "Sample Finder": "Buscador de muestras", 
    "Sample HTML Content": "Muestra de contenido HTML", 
    "Sample JavaScript Content": "Muestra de contenido JavaScript", 
    "Save Draft": "Guardar borrador", 
    "Search Stack Overflow": "Buscar en Stack Overflow", 
    "Sections in Document": "Secciones del documento", 
    "Select a section": "Selecciona una secci\u00f3n", 
    "Select an attachment": "Selecciona un archivo adjunto", 
    "Selected: ": "Seleccionado:\u00a0", 
    "Show old table.": "Mostrar la tabla antigua.", 
    "Something else": "Algo m\u00e1s", 
    "Syntax Highlighter": "Resaltar sintaxis", 
    "Take the survey": "Hacer la encuesta", 
    "Thanks! We'll fix it.": "\u00a1Gracias! Lo solucionaremos.", 
    "The URL you've entered doesn't appear to be valid": "La URL que has introducido parece que no es v\u00e1lida", 
    "Translate it into my language": "Traducir a mi idioma", 
    "URL": "URL", 
    "Uh oh. What would make it better?": "\u00a1Wow! \u00bfEn qu\u00e9 podr\u00eda mejorar?", 
    "What should the sample title be?": "\u00bfCu\u00e1l deber\u00eda ser el t\u00edtulo de la muestra?", 
    "Yes": "S\u00ed", 
    "You have a draft from:": "Tienes un borrador de:", 
    "You must input a valid YouTube video URL.": "Debes introducir una URL v\u00e1lida para un video de YouTube.", 
    "Your browser does not support MathML. A CSS fallback has been used instead.": "Tu navegador no admite MathML. En su lugar, se ha utilizado otro recurso CSS.", 
    "an unknown date": "una fecha desconocida", 
    "discarded": "descartado", 
    "published": "publicado", 
    "restored": "recuperado"
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
    "DATETIME_FORMAT": "j \\d\\e F \\d\\e Y \\a \\l\\a\\s H:i", 
    "DATETIME_INPUT_FORMATS": [
      "%d/%m/%Y %H:%M:%S", 
      "%d/%m/%Y %H:%M:%S.%f", 
      "%d/%m/%Y %H:%M", 
      "%d/%m/%y %H:%M:%S", 
      "%d/%m/%y %H:%M:%S.%f", 
      "%d/%m/%y %H:%M", 
      "%Y-%m-%d %H:%M:%S", 
      "%Y-%m-%d %H:%M:%S.%f", 
      "%Y-%m-%d %H:%M", 
      "%Y-%m-%d"
    ], 
    "DATE_FORMAT": "j \\d\\e F \\d\\e Y", 
    "DATE_INPUT_FORMATS": [
      "%d/%m/%Y", 
      "%d/%m/%y", 
      "%Y-%m-%d"
    ], 
    "DECIMAL_SEPARATOR": ",", 
    "FIRST_DAY_OF_WEEK": "1", 
    "MONTH_DAY_FORMAT": "j \\d\\e F", 
    "NUMBER_GROUPING": "3", 
    "SHORT_DATETIME_FORMAT": "d/m/Y H:i", 
    "SHORT_DATE_FORMAT": "d/m/Y", 
    "THOUSAND_SEPARATOR": ".", 
    "TIME_FORMAT": "H:i", 
    "TIME_INPUT_FORMATS": [
      "%H:%M:%S", 
      "%H:%M:%S.%f", 
      "%H:%M"
    ], 
    "YEAR_MONTH_FORMAT": "F \\d\\e Y"
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

