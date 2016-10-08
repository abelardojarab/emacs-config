/*global process */

var colors = require('colors'),
    _ = require('underscore');
    
var T_ERROR = colors.red( '[ERROR]  '),
    T_WARNING = colors.yellow('[WARNING]'),
    T_INFO = colors.green('[Note]   ');

/**
 * Merge yamls files. Show warning if there is duplicate documentation for same tag, tag-attr, etc
 *
 * @param {Object} yamls[]
 * @param {String} yamls[].filename - filename of yaml file
 * @param {Object} yamls[].data - content of yaml file
 *
 * @returns {Object} o
 * @returns {Object} o.tags - docs for tags
 * @returns {Object} o.attributes - docs for tag-attribute
 * @returns {Object} o.values - docs for attribute values
 */
function mergeYamls(yamls) {
  var resAttributes = {},
      resTags = {},
      resValues = {};

  _.each(yamls, function processOneYaml(yaml) {
    var data = yaml.data;

    if (!data) {
      error('yaml is empty! Read readme how you can use it script');
      process.exit(1);
    }
    
    var attributes = data.attributes;
    var tags = data.tags;

    if (!tags && !attributes) {
      warning('Yaml file have nor "attributes" nor "tags" properties');
    }
    
    if (attributes) {
      if (!_.isArray(attributes)) {
        error('"attributes" should be array');
        process.exit(1);
      }
    }

    if (tags && (!_.isObject(tags) || _.isArray(tags))) {
      error('"tags" should be hash, bug got:');
      console.log(colors.red(JSON.stringify(tags, null, 2)));
      process.exit(1);
    }

    _.each(tags, processTagProps);
    
    _.each(attributes, processAttributesProps);
    
    // funcs

    function processTagProps(doc, tagName) {
      if (resTags[tagName]) {
        warning('Duplicate documentation for tag "' + colors.magenta(tagName) + '", overriding doc! Maybe you should place doc in one place?');
        console.log('Old documentation: ' + colors.red(resTags[tagName]));
        console.log('New documentation: ' + colors.green(doc));
      }
      resTags[tagName] = doc || '';
    }
    
    function processAttributesProps(attribute, i) {

      attrShoutBeObject(attribute);
      attrShouldHaveOneKey(attribute);

      var attributeName = _.keys(attribute)[0],
          properties = attribute[attributeName];
      
      addAttributeIfHaveDoc(attributeName, properties);
      addAttributeValues(attributeName, properties);

      // checks
      function attrShoutBeObject(attribute) {
        if (!_.isObject(attribute) || _.isArray(attribute)) {
          errorAttr('should be Hash but got:');
          console.log(colors.red(JSON.stringify(attribute, null, 2)));        
          process.exit(1);
        }
      }
      
      function attrShouldHaveOneKey(attribute) {
        var keys = _.keys(attribute);
        if (keys.length > 1) {
          errorAttr('keys should be only 1, but got:');
          console.log(keys);
          process.exit(1);
        }
      }

      function errorAttr(msg) {
        error('attribute[' + i +'] ' + msg);
      }
    } // each processAttributesProps

    // check for 'd' property and add into resAttributes
    function addAttributeIfHaveDoc(attributeName, properties) {
      var tagNames = getTagNames(properties),
          doc = properties.d;

      if (!_.isUndefined(doc)) {
        _.each(tagNames, function addTagAttrDocMaybe(tagName) {
          var key = tagName + '-' + attributeName;
          if (!resAttributes[tagName]) {
            resAttributes[tagName] = {};
          }
          if (resAttributes[tagName][attributeName]) {
            warning('Duplicate documentation for tag\'s attribute "' + colors.magenta(key) + '", overriding doc! Maybe you should place doc in one place?');
            console.log('Old documentation: ' + colors.red(resAttributes[tagName][attributeName]));
            console.log('New documentation: ' + colors.green(doc));
          }
          resAttributes[tagName][attributeName] = doc;            
        });
      }
    }

    // check for 'v' property and add to KV into resValues
    function addAttributeValues(attributeName, properties) {
      var tagNames = getTagNames(properties),
          attrValues = properties.v;

      var valuesWithDoc = {};
      // ensure valuesWithDoc is hash, even if it is array
      if (_.isArray(attrValues)) {
        _.each(attrValues, function(v) {
          valuesWithDoc[v] = '';
        });
      } else if (_.isObject(attrValues)) {
        valuesWithDoc = attrValues;
      } else if (!_.isUndefined(attrValues)){ // as string
        valuesWithDoc[''+attrValues] = '';
      }

      if (! _.isEmpty(valuesWithDoc)) {
        _.each(tagNames, function addToAllTagsAttrValues(tagName) {
          var key = tagName + '-' + attributeName;
          _.each(valuesWithDoc, function appendValues(doc, attrValue) {
            if (! resValues[key]) {
              resValues[key] = {};
            }

            var oldDoc = resValues[key][attrValue];
            if (! _.isEmpty(oldDoc)) {
              // override only if new doc is not empty text
              if (! _.isEmpty(doc)) {
                warning('Duplicate documentation for value "' + colors.magenta(key + '.' + attrValue) + '", overriding doc! Maybe you should place doc in one place?');
                console.log('Old documentation: ' + colors.red(oldDoc));
                console.log('New documentation: ' + colors.green(doc));
                resValues[key][attrValue] = doc || '';
              }
            } else {
              resValues[key][attrValue] = doc || '';
            }            
          });
        });
      }
    }
    
    // return arraified 't' property or default ['global']
    function getTagNames(properties) {
      var tags = properties.t || ['global'];
      if (! _.isArray(tags)) {
        tags = [tags];
      }
      return tags;
    }
    
    function warning(msg) {
      console.log('%s "%s": ' + msg, T_WARNING, colors.magenta(yaml.filename));
    }

    function error(msg) {
      console.log('%s "%s": ' + msg, T_ERROR, colors.magenta(yaml.filename));
    }

  });

  return {
    values: resValues,
    attributes: resAttributes,
    tags: resTags
  };
}

module.exports = mergeYamls;
