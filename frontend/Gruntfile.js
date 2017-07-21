var JS_VENDOR_PATH = 'js/vendor',
    CSS_VENDOR_PATH = 'css/vendor';

module.exports = function(grunt) {

  grunt.loadNpmTasks('grunt-bowercopy');
  grunt.loadNpmTasks('grunt-contrib-clean');

  grunt.initConfig({
    clean: {
      'vendor-js': JS_VENDOR_PATH,
      'vendor-css': CSS_VENDOR_PATH
    },

    bowercopy: {
        options: {
        },
        js: {
          options: {
            destPrefix: JS_VENDOR_PATH
          },
          files: {
            'todomvc-common.js' : 'todomvc-common/base.js',
            'jquery.js' : 'jquery/jquery.js',
            'underscore.js' : 'underscore/underscore.js',
            'backbone.js' : 'backbone/backbone.js'
          }
        },
        css: {
          options: {
            destPrefix: CSS_VENDOR_PATH
          },
          files: {
            'todomvc-common.css':'todomvc-common/base.css'
          }
        }
      }
  });

  grunt.registerTask('default', ['clean','bowercopy']);

};
