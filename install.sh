#!/usr/bin/env bash

mkdir -p /Library/WebServer/Documents/webcomparator
mkdir -p /Library/WebServer/Documents/webcomparator/js
mkdir -p /Library/WebServer/Documents/webcomparator/js/
mkdir -p /Library/WebServer/Documents/webcomparator/css
cp  html/*.html /Library/WebServer/Documents/webcomparator/
cp  html/js/*.js /Library/WebServer/Documents/webcomparator/js/
cp -r  html/js/i18n /Library/WebServer/Documents/webcomparator/js/
cp  html/css/*.css /Library/WebServer/Documents/webcomparator/css/
cp dist/build/query.cgi/query.cgi /Library/WebServer/CGI-Executables/
cp dist/build/edit.cgi/edit.cgi /Library/WebServer/CGI-Executables/
cp data/Tangkhul3.db /tmp
chmod a+rw /tmp/Tangkhul3.db
