

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
    "Add more details": "Adicionar mais detalhes", 
    "Add or improve examples": "Adicionar ou melhorar exemplos", 
    "Article Title Lookup / Link Text": "Busca do t\u00edtulo do artigo / Texto do link", 
    "Aspect ratio": "Formato da tela", 
    "Attachments": "Anexos", 
    "Autosave disabled.": "Salvar automaticamente desabilitado.", 
    "Autosave enabled.": "Salvar automaticamente habilitado.", 
    "CSS Content": "Conte\u00fado CSS", 
    "Choose one...": "Escolha um...", 
    "Close": "Fechar", 
    "Close notification": "Fechar a notifica\u00e7\u00e3o", 
    "Close submenu": "Fechar o submenu", 
    "Create a Redirect": "Criar um redirecionamento", 
    "Default": "Padr\u00e3o", 
    "Details": "Detalhes", 
    "Did this page help you?": "Esta p\u00e1gina ajudou?", 
    "Discard the draft": "Descartar rascunho", 
    "Document": "Documento", 
    "Draft": "Rascunho", 
    "Embed YouTube Video": "Integrar v\u00eddeo do YouTube", 
    "Enable autosave.": "Habilitar salvar automaticamente.", 
    "Fix incorrect information": "Corrigir informa\u00e7\u00f5es incorretas", 
    "HTML Content": "Conte\u00fado HTML", 
    "How to read CSS syntax.": "Como ler uma sintaxe CSS.", 
    "Insert Code Sample Template": "Inserir template de exemplo de c\u00f3digo", 
    "Insert Code Sample iFrame": "Inserir um exemplo de c\u00f3digo iFrame", 
    "JavaScript Content": "Conte\u00fado JavaScript", 
    "Launch": "Iniciar", 
    "Locate a YouTube Video": "Localizar um v\u00eddeo do YouTube", 
    "MDN Redirect": "Redirecionamento MDN", 
    "Make explanations clearer": "Explica\u00e7\u00f5es mais claras", 
    "More about the beta.": "Mais sobre o beta.", 
    "My search should have led to a different article": "Minha pesquisa deveria ter me levado a um artigo diferente", 
    "Never ask me again": "N\u00e3o me pergunte de novo", 
    "New compatibility tables are in beta ": "As novas tabelas de compatibilidade est\u00e3o em beta\u00a0", 
    "New interest...": "Novo interesse...", 
    "New tag...": "Nova etiqueta...", 
    "No": "N\u00e3o", 
    "No Highlight": "Sem destaque", 
    "No attachments available": "Sem anexos dispon\u00edveis", 
    "No selection": "Nada selecionado", 
    "Open": "Abrir", 
    "Open implementation notes": "Abrir notas de implementa\u00e7\u00e3o", 
    "Paste YouTube Video URL": "Colar URL do v\u00eddeo do YouTube", 
    "Report an error.": "Informar um erro.", 
    "Reported. Thanks!": "Informado. Obrigado!", 
    "Restore the draft content": "Restaurar o conte\u00fado do rascunho", 
    "Return to compatibility table.": "Retornar \u00e0 tabela de compatibilidade.", 
    "Sample CSS Content": "Exemplo de conte\u00fado CSS", 
    "Sample Finder": "Buscador de amostras", 
    "Sample HTML Content": "Exemplo de conte\u00fado HTML", 
    "Sample JavaScript Content": "Exemplo de conte\u00fado JavaScript", 
    "Save Draft": "Salvar Rascunho", 
    "Search Stack Overflow": "Pesquisar no Stack Overflow", 
    "Sections in Document": "Se\u00e7\u00f5es do documento", 
    "Select a section": "Selecionar uma se\u00e7\u00e3o", 
    "Select an attachment": "Selecionar um anexo", 
    "Selected: ": "Selecionado:\u00a0", 
    "Show old table.": "Mostrar tabela antiga.", 
    "Something else": "Algo mais", 
    "Syntax Highlighter": "Destacar sintaxe", 
    "Take the survey": "Responda a pesquisa", 
    "Thanks! We'll fix it.": "Obrigado! Vamos consertar.", 
    "The URL you've entered doesn't appear to be valid": "A URL informada n\u00e3o parece ser v\u00e1lida", 
    "Translate it into my language": "Traduzir para o meu idioma", 
    "URL": "URL", 
    "Uh oh. What would make it better?": "Opa. O que poderia melhorar?", 
    "What should the sample title be?": "Qual ser\u00e1 o t\u00edtulo do exemplo?", 
    "Yes": "Sim", 
    "You have a draft from:": "Voc\u00ea tem um rascunho de:", 
    "You must input a valid YouTube video URL.": "Voc\u00ea deve fornecer um URL v\u00e1lido de um v\u00eddeo do YouTube.", 
    "Your browser does not support MathML. A CSS fallback has been used instead.": "Seu navegador n\u00e3o suporta MathML. Ao inv\u00e9s disso, foi utilizado um recurso CSS.", 
    "an unknown date": "uma data desconhecida", 
    "published": "publicado", 
    "restored": "restaurado"
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
    "DATETIME_FORMAT": "j \\d\\e F \\d\\e Y \u00e0\\s H:i", 
    "DATETIME_INPUT_FORMATS": [
      "%d/%m/%Y %H:%M:%S", 
      "%d/%m/%Y %H:%M:%S.%f", 
      "%d/%m/%Y %H:%M", 
      "%d/%m/%Y", 
      "%d/%m/%y %H:%M:%S", 
      "%d/%m/%y %H:%M:%S.%f", 
      "%d/%m/%y %H:%M", 
      "%d/%m/%y", 
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
    "FIRST_DAY_OF_WEEK": "0", 
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

