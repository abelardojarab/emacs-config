

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
    "Add more details": "Tambahkan lebih banyak detail", 
    "Add or improve examples": "Tambah atau tingkatkan contoh", 
    "Article Title Lookup / Link Text": "Pencarian Judul Artikel / Tautan Teks", 
    "Aspect ratio": "Rasio aspek", 
    "Attachments": "Lampiran", 
    "CSS Content": "Konten CSS", 
    "Choose one...": "Pilih satu...", 
    "Close": "Tutup", 
    "Close notification": "Tutup notifikasi", 
    "Close submenu": "Tutup submenu", 
    "Create a Redirect": "Membuat Pengalihan", 
    "Default": "Bawaan", 
    "Details": "Rincian", 
    "Did this page help you?": "Apakah halaman ini membantu anda?", 
    "Discard the draft": "Membuang rancangan", 
    "Document": "Dokumen", 
    "Embed YouTube Video": "Melekatkan Video YouTube", 
    "Fix incorrect information": "Memperbaiki kesalahan informasi", 
    "HTML Content": "Konten HTML", 
    "How to read CSS syntax.": "Bagaimana cara membaca sintaksis CSS.", 
    "Insert Code Sample Template": "Menyisipkan Contoh Kode Template", 
    "Insert Code Sample iFrame": "Menyisipkan Contoh Kode iFrame", 
    "JavaScript Content": "Konten JavaScript", 
    "Launch": "Luncurkan", 
    "Locate a YouTube Video": "Menemukan sebuah Video YouTube", 
    "MDN Redirect": "Pengalihan MDN", 
    "Make explanations clearer": "Membuat penjelasan lebih jelas", 
    "More about the beta.": "Lebih lanjut tentang versi beta.", 
    "My search should have led to a different article": "Pencarian saya harus mengarahkan kepada artikel yang berbeda", 
    "Never ask me again": "Jangan pernah bertanya pada saya lagi", 
    "New compatibility tables are in beta ": "Tabel kompatibilitas baru tersedia dalam versi beta ", 
    "New interest...": "Minat baru...", 
    "New tag...": "Tag baru...", 
    "No": "Tidak", 
    "No Highlight": "Tak Ada Penanda", 
    "No attachments available": "Lampiran tidak tersedia", 
    "No selection": "Tidak ada pilihan", 
    "Open": "Buka", 
    "Open implementation notes": "Catatan implementasi terbuka", 
    "Paste YouTube Video URL": "Menyalin URL Video YouTube", 
    "Report an error.": "Melaporkan kesalahan.", 
    "Reported. Thanks!": "Dilaporkan. Terima kasih!", 
    "Restore the draft content": "Mengembalikan konten rancangan", 
    "Return to compatibility table.": "Kembali ke tabel kompatibilitas.", 
    "Sample CSS Content": "Contoh Konten CSS", 
    "Sample Finder": "Contoh Finder", 
    "Sample HTML Content": "Contoh Konten HTML", 
    "Sample JavaScript Content": "Contoh Konten JavaScript", 
    "Search Stack Overflow": "Cari Stack Overflow", 
    "Sections in Document": "Bagian dalam Dokumen", 
    "Select a section": "Memilih bagian", 
    "Select an attachment": "Pilih lampiran", 
    "Selected: ": "Terpilih: ", 
    "Show old table.": "Tampilkan tabel lama.", 
    "Something else": "Lainnya", 
    "Syntax Highlighter": "Penanda Sintaksis", 
    "Take the survey": "Isilah survei", 
    "Thanks! We'll fix it.": "Terima kasih! Kami akan memperbaiki ini.", 
    "The URL you've entered doesn't appear to be valid": "URL yang anda masukkan sepertinya tidak benar", 
    "Translate it into my language": "Menerjemahkannya ke dalam bahasa saya", 
    "URL": "URL", 
    "Uh oh. What would make it better?": "Uh oh. Bagaimana membuat ini lebih baik?", 
    "What should the sample title be?": "Apa yang sebaiknya menjadi contoh judul?", 
    "Yes": "Ya", 
    "You have a draft from:": "Anda memiliki rancangan dari:", 
    "You must input a valid YouTube video URL.": "Anda harus memasukkan URL Video YouTube yang benar.", 
    "Your browser does not support MathML. A CSS fallback has been used instead.": "Peramban anda tidak mendukung MathML. Sebuah sandaran CSS telah digunakan sebagai gantinya.", 
    "an unknown date": "tanggal yang tidak diketahui"
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
    "DATETIME_FORMAT": "j N Y, G.i", 
    "DATETIME_INPUT_FORMATS": [
      "%d-%m-%Y %H.%M.%S", 
      "%d-%m-%Y %H.%M.%S.%f", 
      "%d-%m-%Y %H.%M", 
      "%d-%m-%Y", 
      "%d-%m-%y %H.%M.%S", 
      "%d-%m-%y %H.%M.%S.%f", 
      "%d-%m-%y %H.%M", 
      "%d-%m-%y", 
      "%m/%d/%y %H.%M.%S", 
      "%m/%d/%y %H.%M.%S.%f", 
      "%m/%d/%y %H.%M", 
      "%m/%d/%y", 
      "%m/%d/%Y %H.%M.%S", 
      "%m/%d/%Y %H.%M.%S.%f", 
      "%m/%d/%Y %H.%M", 
      "%m/%d/%Y", 
      "%Y-%m-%d %H:%M:%S", 
      "%Y-%m-%d %H:%M:%S.%f", 
      "%Y-%m-%d %H:%M", 
      "%Y-%m-%d"
    ], 
    "DATE_FORMAT": "j N Y", 
    "DATE_INPUT_FORMATS": [
      "%d-%m-%y", 
      "%d/%m/%y", 
      "%d-%m-%Y", 
      "%d/%m/%Y", 
      "%d %b %Y", 
      "%d %B %Y", 
      "%Y-%m-%d"
    ], 
    "DECIMAL_SEPARATOR": ",", 
    "FIRST_DAY_OF_WEEK": "1", 
    "MONTH_DAY_FORMAT": "j F", 
    "NUMBER_GROUPING": "3", 
    "SHORT_DATETIME_FORMAT": "d-m-Y G.i", 
    "SHORT_DATE_FORMAT": "d-m-Y", 
    "THOUSAND_SEPARATOR": ".", 
    "TIME_FORMAT": "G.i", 
    "TIME_INPUT_FORMATS": [
      "%H.%M.%S", 
      "%H.%M", 
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

