

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
    "Add more details": "Adicionar mais detalhes", 
    "Add or improve examples": "Adicionar ou melhorar exemplos", 
    "Article Title Lookup / Link Text": "T\u00edtulo do artigo para pesquisa / Texto da liga\u00e7\u00e3o", 
    "Aspect ratio": "Rela\u00e7\u00e3o de aspeto", 
    "Attachments": "Anexos", 
    "Autosave disabled.": "Guardar automaticamente desativado.", 
    "Autosave enabled.": "Guardar automaticamente ativado.", 
    "CSS Content": "Conte\u00fado CSS", 
    "Choose one...": "Escolha uma op\u00e7\u00e3o...", 
    "Close": "Fechar", 
    "Close notification": "Fechar notifica\u00e7\u00e3o", 
    "Close submenu": "Fechar sub-menu", 
    "Create a Redirect": "Criar um redirecionamento", 
    "Default": "Predefinido", 
    "Details": "Detalhes", 
    "Did this page help you?": "Esta p\u00e1gina ajudou-o?", 
    "Discard the draft": "Descartar o rascunho", 
    "Document": "Documento", 
    "Draft": "Rascunho", 
    "Embed YouTube Video": "Incorporar v\u00eddeo do YouTube", 
    "Enable autosave.": "Ativar guardar automaticamente.", 
    "Fix incorrect information": "Corrigir informa\u00e7\u00e3o incorreta", 
    "HTML Content": "Conte\u00fado HTML", 
    "How to read CSS syntax.": "Como ler a sintaxe CSS.", 
    "Insert Code Sample Template": "Inserir modelo do exemplo de c\u00f3digo", 
    "Insert Code Sample iFrame": "Inserir iFrame de exemplo de c\u00f3digo", 
    "JavaScript Content": "Conte\u00fado JavaScript", 
    "Launch": "Executar", 
    "Locate a YouTube Video": "Localizar um v\u00eddeo do YouTube", 
    "MDN Redirect": "Redirecionamento MDN", 
    "Make explanations clearer": "Tornar as explica\u00e7\u00f5es mais claras", 
    "More about the beta.": "Mais sobre o beta.", 
    "My search should have led to a different article": "A minha pesquisa deveria ter encaminhado para um artigo diferente", 
    "Never ask me again": "N\u00e3o perguntar novamente", 
    "New compatibility tables are in beta ": "Novas tabelas de compatibilidade est\u00e3o em beta ", 
    "New interest...": "Novo interesse...", 
    "New tag...": "Nova etiqueta...", 
    "No": "N\u00e3o", 
    "No Highlight": "Sem destaque", 
    "No attachments available": "Sem anexos dispon\u00edveis", 
    "No selection": "Sem sele\u00e7\u00e3o", 
    "Open": "Abrir", 
    "Open implementation notes": "Abrir notas de implementa\u00e7\u00e3o", 
    "Paste YouTube Video URL": "Colar URL de um v\u00eddeo do YouTube", 
    "Report an error.": "Reporte um erro.", 
    "Reported. Thanks!": "Reportado. Obrigado!", 
    "Restore the draft content": "Restaurar o conte\u00fado do rascunho", 
    "Return to compatibility table.": "Voltar para a tabela de compatibilidade.", 
    "Sample CSS Content": "Conte\u00fado CSS do exemplo", 
    "Sample Finder": "Pesquisa de exemplos", 
    "Sample HTML Content": "Conte\u00fado HTML do exemplo", 
    "Sample JavaScript Content": "Conte\u00fado JavaScript do exemplo", 
    "Save Draft": "Guardar rascunho", 
    "Search Stack Overflow": "Pesquisar o Stack Overflow", 
    "Sections in Document": "Sec\u00e7\u00f5es num documento", 
    "Select a section": "Selecionar uma sec\u00e7\u00e3o", 
    "Select an attachment": "Selecionar um anexo", 
    "Selected: ": "Selecionado: ", 
    "Show old table.": "Mostrar tabela antiga.", 
    "Something else": "Outra coisa", 
    "Syntax Highlighter": "Destaque de sintaxe", 
    "Take the survey": "Preencha o inqu\u00e9rito", 
    "Thanks! We'll fix it.": "Obrigado! Iremos corrigir isto.", 
    "The URL you've entered doesn't appear to be valid": "O URL que introduziu parece ser inv\u00e1lido", 
    "Translate it into my language": "Traduzir para o meu idioma", 
    "URL": "URL", 
    "Uh oh. What would make it better?": "Ent\u00e3o, o que podemos fazer para a melhorar?", 
    "What should the sample title be?": "Qual deve ser o t\u00edtulo do exemplo?", 
    "Yes": "Sim", 
    "You have a draft from:": "Tem um rascunho de:", 
    "You must input a valid YouTube video URL.": "Deve introduzir um URL de v\u00eddeo do YouTube v\u00e1lido.", 
    "Your browser does not support MathML. A CSS fallback has been used instead.": "O seu navegador n\u00e3o suporta MathML. Ser\u00e1 utilizado um mecanismo alternativo de substitui\u00e7\u00e3o em CSS.", 
    "an unknown date": "uma data desconhecida", 
    "discarded": "descartado", 
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
      "%Y-%m-%d %H:%M:%S", 
      "%Y-%m-%d %H:%M:%S.%f", 
      "%Y-%m-%d %H:%M", 
      "%Y-%m-%d", 
      "%d/%m/%Y %H:%M:%S", 
      "%d/%m/%Y %H:%M:%S.%f", 
      "%d/%m/%Y %H:%M", 
      "%d/%m/%Y", 
      "%d/%m/%y %H:%M:%S", 
      "%d/%m/%y %H:%M:%S.%f", 
      "%d/%m/%y %H:%M", 
      "%d/%m/%y"
    ], 
    "DATE_FORMAT": "j \\d\\e F \\d\\e Y", 
    "DATE_INPUT_FORMATS": [
      "%Y-%m-%d", 
      "%d/%m/%Y", 
      "%d/%m/%y"
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

