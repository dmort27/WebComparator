#!/usr/bin/env bash

CGIBIN = /Library/WebServer/CGI-Executables
CGI = /Library/WebServer/Documents
DB = /var/webcomparator

mkdir -p $CGI/webcomparator
mkdir -p $CGI/webcomparator/js
mkdir -p $CGI/webcomparator/css
cp  html/*.html $CGI/webcomparator/
cp  html/js/*.js $CGI/webcomparator/js/
cp -r  html/js/i18n $CGI/webcomparator/js/
cp  html/css/*.css $CGI/webcomparator/css/
cp dist/build/query.cgi/query.cgi $CGIBIN
cp dist/build/edit.cgi/edit.cgi $CGIBIN
mkdir -p $DB
cp data/Tangkhul3.db $DB
chmod a+rw $DB/Tangkhul3.db
