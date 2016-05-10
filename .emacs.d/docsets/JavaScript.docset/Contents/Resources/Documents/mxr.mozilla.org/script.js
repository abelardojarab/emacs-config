function changetarget(baseurl, thistree) {
 if (!document.getElementById)
  return;

 var form;
 var target = document.getElementById('tree');
 if (form = document.getElementById('source')) {
  var parent = baseurl.substr(0, baseurl.length - thistree.length);
  var path = document.location.href;
  if (parent == path.substr(0, parent.length)) {
   path = path.substring(baseurl.length);
   var newtree = target.value + '/';
   if (thistree != newtree)
    document.location = parent + newtree + path;
  }
 } else if (form = document.getElementById('search')) {
  form.action='../'+target.value+'/search';
 } else if (form = document.getElementById('find')) {
  form.action='../'+target.value+'/find';
 } else if (form = document.getElementById('ident')) {
  form.action='../'+target.value+'/ident';
 }
}

function changefindpreset() {
 if (document.getElementById) {
  var find = document.getElementById('find');
  var findi = document.getElementById('findi');
  if (findi.selectedIndex > 0)
   find.value = findi.value;
 }
}

function changerev() {
 var form = document.getElementById('revs');
 var target = document.getElementById('rev');
 var newrev = target.value;
 var oldquery = document.location.search;
 var newquery = oldquery.replace(/^\?/, '').replace(/((?:^|[&;])rev=)(?:[^&;]*(?:[&;]+|$))/, '');
 newquery = 'rev='+newrev + (newquery != '' ? '&' + newquery : '');
 document.location.search = newquery;
}

function updateBonsaiBlameHash(node, event) {
  if (!document.location.hash)
    return;
  var marks = [];
  if (/[?&;]mark=([-,0-9]+)(?:[&;]|$)/.test(document.location.search))
    marks.push(RegExp.$1);
  marks.push(document.location.hash.replace("#",''));
  marks = "&mark="+marks.join(',');
  var anchor = document.location.hash;
  node.href =
    node.href.replace(/&amp;mark=\d*|$/, marks)
             .replace(/#.*|$/, anchor);
}

function updateHgBlameHash(node, event) {
  if (!document.location.hash)
    return;
  var anchor = document.location.hash;
  anchor = anchor.replace("#", "#l");
  node.href =
    node.href.replace(/#.*|$/, anchor);
}
