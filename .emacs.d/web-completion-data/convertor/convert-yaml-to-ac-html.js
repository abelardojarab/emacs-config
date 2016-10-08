/*global process */

var argv = require('minimist')(process.argv.slice(2));
var mergeYamls = require('./lib/merger.js'),
    colors = require('colors'),
    mkdirp = require('mkdirp'),
    path = require('path'),
    yaml = require('js-yaml'),
    fs = require('fs'),
    _ = require('underscore');

var MAX_DOC_SIZE = 250;         // Size of documentation. If greater than create separate doc file

if (! argv._.length || !argv.out) {
  console.log('Usage: build.js --out <dir> file1.yaml ...');
  process.exit(0);
}

var FILE_TAG_LIST = 'html-tag-list',
    DIR_TAGS_DOC = 'html-tag-short-docs',
    DIR_ATTRIBUTES_VALUES = 'html-attributes-complete',
    DIR_ATTRIBUTES = 'html-attributes-list',
    DIR_ATTRIBUTES_LARGE_DOC = 'html-attributes-short-docs';
    
function main(argv) {
  var yamls = _.map(argv._, loadYAML);
  var data = mergeYamls(yamls);

  createStuffFiles(data, argv.out);

  console.log(colors.green('Done.'));
}

function loadYAML(file) {
  var data = fs.readFileSync(file, 'utf-8');
  
  return {
    filename: file,
    data: yaml.safeLoad(data)
  };
}

// create html stuff. They are sorted ABC
function createStuffFiles(data, dir) {
  var tags = data.tags,
      tagAttributes = data.attributes,
      attributeValues = data.values;

  var tagFile = path.join(dir, FILE_TAG_LIST);
  var tagDocDir = path.join(dir, DIR_TAGS_DOC);
  
  mkdirp.sync(dir);

  // html tags with docs
  if (! _.isEmpty(tags)) {
    mkdirp.sync(tagDocDir);
    var tagsData = _
          .keys(tags)
          .sort()
          .join('\n');
    
    fs.writeFileSync(tagFile, tagsData);
    created(tagFile);

    var sortedTagNames = _.keys(tags).sort();

    sortedTagNames.forEach(function createTagsDoc(tagName) {
      var file = path.join(tagDocDir, tagName);
      var doc = tags[tagName];
      
      if (!_.isEmpty(doc)) {
        fs.writeFileSync(file, doc);
        created(file);
      }
    });

    console.log('');
  }

  if (!_.isEmpty(tagAttributes)) {
    var attributesDir = path.join(dir, DIR_ATTRIBUTES),
        attributesDirDoc = path.join(dir, DIR_ATTRIBUTES_LARGE_DOC);

    mkdirp.sync(attributesDir);
    mkdirp.sync(attributesDirDoc);

    // build attribute list for html tags
    _.each(tagAttributes, function createAttribute(tagAttributes, tagName) {
      var tagAttributesList = [];

      _.each(tagAttributes, function processEachAttribute(doc, attributeName) {
        var docSize = _.size(doc),
            docAppender = '',   // will be `' ' + doc` if doc is short
            filename;
        
        if (docSize > MAX_DOC_SIZE) {
          filename = path.join(attributesDirDoc, tagName + '-' + attributeName);
          fs.writeFileSync(filename, doc);
          created(filename);
        } else if (docSize) {
          docAppender = ' ' + stringifyDoc(doc);
        }

        tagAttributesList.push(attributeName + docAppender);
      });

      var tagAttributeData = tagAttributesList.sort().join('\n'),
          filename = path.join(attributesDir, tagName);

      fs.writeFileSync(filename, tagAttributeData);
      created(filename);      
    });
  }

  if (!_.isEmpty(attributeValues)) {
    var attributeValuesDir = path.join(dir, DIR_ATTRIBUTES_VALUES);
    mkdirp.sync(attributeValuesDir);

    _.each(attributeValues, function createValues(values, tagAttributeName) {
      var valueData = _.map(values, function(doc, valueName) {
        if (! _.isEmpty(doc)) {
          return valueName + ' ' + stringifyDoc(doc);
        } else {
          return valueName;
        }
      });
      valueData = valueData.sort().join('\n');

      var filename = path.join(attributeValuesDir, tagAttributeName);
      fs.writeFileSync(filename, valueData);
      created(filename);
    });
  }
}

function stringifyDoc(text) {
  return text.replace(/\n$/, '').replace(/\n/g, '\\n');
}

function created(msg) {
  console.log(colors.green('Created') + ' ' + msg);  
}

main(argv);
