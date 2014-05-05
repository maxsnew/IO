#!/usr/bin/env node

// imports
var run_io    = require('./run-io.js').run_io;
var elmloader = require('elmloader');
var exec      = require('child_process').exec;
var path      = require('path');
var usage = function() {
    console.log("USAGE: elm-io <ELM_LIB.elm>");
    process.exit(1);    
}
var check_err = function(msg, err, stdout, stderr) {
    // console.log(stdout);
    if (err !== null) {
        console.log(msg);
        console.log(stderr);
        process.exit(err);
    }
}
var args = process.argv.slice(2);
var elm_lib  = (args.length !== 1) ? usage() : args[0];

if (elm_lib.slice(-4) !== '.elm') {
    console.log('Input must be a .elm file');
    usage();
}
var base = elm_lib.slice(0,-4);
var compiled = path.join('build', base + '.js');
exec('elm --show-runtime',
     function(err, stdout, stderr) {
         var runtime = stdout.toString().trim();
         check_err("Error finding runtime", err, stdout, stderr);
         exec('elm -mo ' + elm_lib,
              function(err, stdout, stderr) {
                  check_err("Error building" + elm_lib, err, stdout, stderr)
                  var Elm = elmloader(runtime, compiled);
                  run_io(Elm);
              })
     });
