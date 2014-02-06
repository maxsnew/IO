#!/usr/bin/env node

var jsdom = require("jsdom");
var callback = function(errors, window) {
    var document = window.document;
