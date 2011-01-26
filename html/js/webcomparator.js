/* WHEN DOCUMENT IS READY, EXECUTE */

$(document).ready(
    function () {

        /* GLOBAL SITE SETTINGS */

        var cgiRoot = '/cgi-bin/';
        
        // Formatters and unformatters
	var protoformFormat = function(s) { return ('*' + s); };
	var protoformUnformat = function(s) { return s.substring(1, s.length); };
	var glossFormat = function(s) { return ('‘' + s + '’'); };
        var generalUnformat = function(s) { return s.substring(1, s.length - 1); };
	var reflexFormat = function(s, options, row) { 
	    var parts = s.replace(/ /g, ' -').split('-');
	    if (row.cogmorph) {
		row.cogmorph.split(",").map(function(x) {
		    var pair = x.split(":");
		    parts[pair[0]] = "<span class='tagged' title='" + 
			pair[1] + "'>" + 
			parts[pair[0]] + "</span>";
		});
	    }
	    return parts.join("-").replace(/ -/g, ' ');
	};
	var reflexUnformat = function(s) { return s; };

        /* MORPH-PICKER */
        
        // Instantiate the morph-picker dialog
	var morphDialog =
            $("#morph-picker-dialog")
	    .dialog({
		autoOpen: false,
		closeOnEscape: true,
                modal: true,
                overlay: {background: "black", opacity: 0.8},
		title: "Select morph",
		buttons: {
                    
		    "Save" : function() { 
			var data = $("#morph-picker").data();
			$.ajax({ url: cgiRoot + "edit.cgi", 
				 data: {oper: "setmorphind", 
					refid: data.refid, 
					cogsetid: data.cogsetid, 
					morphind: data.morphind}, 
				 type: "POST",
				 success: function() { updateCogSet(data.cogsetid); }
			       });
                        $(this).dialog("close");
		    },
                    
		    "Reparse" : function() {
                        reparseReflex();
                    },
                    
                    "Remove" : function() {
                        var data = $("#morph-picker").data();
                        $.ajax({
                            url: cgiRoot + "edit.cgi",
                            data: {oper: "removefromset",
                                   cogsetid: data.cogsetid,
                                   refid: data.refid
                                  },
                            type: "POST",
                            success: function () {
                                updateCogSet( data.cogsetid );
                            }
                            
                        });
                        $(this).dialog("close");
                    },
                    
		    "Cancel": function() {
                        $(this).dialog("close");
                    }
	        }
	    });

        // Display the morphs in the morph picker dialog.
	var showMorphs = function(form, morphind) {

	    var morphs = form.replace(/ /g, ' -').split('-');
	    var newMorphs = [];

	    $.each(morphs, function(i, m) {
		var span;
		if (morphind == i) {
		    span = "<span title='" + i + "' class='morph indexed'>" + m + "</span>";
		} else {
		    span = "<span title='" + i + "' class='morph'>" + m + "</span>";
		}
		newMorphs.push(span);
	    });

	    $("#morph-picker").empty()
		.append(newMorphs.join("-"));

	    $("#morph-picker .morph").click(function() {
		$("#morph-picker span.morph").removeClass("indexed");
		$(this).addClass("indexed");
		$("#morph-picker").data("morphind", this.title);
	    }).dblclick(function() {
                reparseReflex();
            });
	};

        // Allow the user to re-divide a form into morphs.
	var reparseReflex = function() {
	    var data = $("#morph-picker").data();
	    $("#morph-picker").empty().html("<input id='form-reparse'></input>");
	    $("#form-reparse").val(data.form);
	    $("#form-reparse").keypress(function(e) {
		var c = e.which ? e.which : e.keyCode;
		if (c == 13 || c == 10) {
		    e.preventDefault();
		    data.form = $(this).val();
		    $("#morph-picker").empty();
		    $("#morph-picker").data(data);
		    $.ajax( {url: cgiRoot + "edit.cgi", 
			     data: { oper: "edit", 
				     table: "reflexes", 
				     id: data.refid,
				     form: data.form }, 
			     type: "POST",
			     success: function() { showMorphs(data.form, data.morphind); }
			    });
		    
		};
	    });
	};
        
	// Refreshes the display of the current cognate set.
	var updateCogSet = function(cogsetid) {

            // Set the value of cogsetid in two useful locations.
	    $("body").data("cogsetid", cogsetid);
	    $("#cogset-box").data("cogsetid", cogsetid);
            
            // URL for getting cogset JSON.
            var url = cgiRoot + "query.cgi?qtype=cogset&cogsetid=" + cogsetid;
            
            // Callback which does most of the actual work of displaying cognet set and setting up events on it.
	    var updateCogSetP = function(data) {
		$("#cogset-tbody").empty();
		$.each(data, function(i, row) {
		    var langid = row[0];
		    var langname = row[1];
		    var forms = row[2];
		    var glosses = row[3];
		    var ids = row[4];
		    var morphinds = row[5];
		    $("#cogset-tbody").append("<tr><td class='langname'>" + langname + "</td><td class='reflex'></td>");
		    if (forms) {
			$.each(forms, function(j, form) {
			    var refid = ids[j];
			    var gloss = glosses[j];
			    var morphind = morphinds[j];
			    var morphs = form.replace(/ /g, ' -').split('-');
			    var formData = { form: form, 
					     refid: refid, 
					     cogsetid: cogsetid,
					     morphs: morphs, 
					     morphind: morphind, 
					     gloss: gloss, 
					     formchanged: false};
			    morphs[morphind] = "<span class='indexed'>" + morphs[morphind] + "</span>";
			    var newForm = morphs.join("-").replace(/ -/g, ' ').replace(/-+$/, '');
			    var thisForm = $('<div></div>')
				.html("<i>" + newForm + "</i> ‘" + gloss + "’ ")
				.addClass("ref")
				.data(formData);
			    $("#cogset-tbody tr:last td:last").append(thisForm);
			});
		    }
		});

                // Add classes to parts of the cogset display for presentation purposes. Needs work.
		$("#cogset-box").addClass("ui-jqgrid ui-widget ui-widget-content ui-corner-all");
		$("#cogset-head").addClass("ui-jqgrid-titlebar ui-widget-header ui-corner-all ui-helper-clearfix");
		$("#cogset-head span").addClass("ui-jqgrid-title");
		$("#cogset-box tbody").addClass("ui-jqgrid-bdiv");
		$("#cogset-box tbody tr").addClass("ui-widget-content jqgrow ui-row-ltr");
		$("#cogset-table thead").addClass("ui-state-default ui-jqgrid-hdiv");
		$("#cogset-table thead tr").addClass("ui-jqgrid-labels");
		$("#cogset-table thead th").addClass("ui-jqgrid-labels ui-state-default ui-th-column ui-th-ltr");

                // Open the morph picker dialog when a form is double-clicked.
		$("div.ref").dblclick(function() {
		    var data = $(this).data();
		    $("#morph-picker").data(data);		    
		    showMorphs(data.form, data.morphind);
                    morphDialog.show();
		    morphDialog.dialog("open");
		    return false;
		});
	    };

            // Request the JSON to display the set and, when it is returned, display it.
	    $.getJSON(url, updateCogSetP);
	    
	};


        // Create the cogsets table (which lists the protoforms and glosses).
	var initCogSets = function() {

	    var cogsets = $("#cogsets").jqGrid({
		jsonReader : { repeatitems: false, id: "cogsetid" },
		url: cgiRoot + 'query.cgi?qtype=cogsets',
		editurl: cgiRoot + 'edit.cgi',
		datatype: 'json',
		mtype: 'GET',
		height: 500,
		width: 400,
		colNames: ["Set ID", "Proto-form", "Gloss"],
		colModel: [
                    { name:'cogsetid', index:'cogsetid', width:50, hidden: false, search: false },
		    { name: 'form', index:'form', width:50, align: 'left', editable: true, editoptions: {size: 40},
		      formatter:protoformFormat, unformat:protoformUnformat },
		    { name: 'gloss', index:'gloss', width:100, align: 'left', editable: true, editoptions: {size: 40},
		      formatter:glossFormat, unformat:generalUnformat }
		],
		page: 1,
		pager: '#cogsets-pager',
		rowNum: 100,
		rowList: [25, 50, 100, 200],
		sortname: 'cogsetid',
		sortorder: 'asc',
		viewrecords: true,
		caption: 'Proto-Forms',
		onSelectRow: function(cogsetid) { 
		    updateCogSet(cogsetid);
		    $("body").data("cogsetid", cogsetid);
		    console.log($("body").data());
		}
	    }).navGrid('#cogsets-pager', {view: false, search: false}, 
		       {editData: {table: 'cogsets'}}, 
		       {editData: {table: 'cogsets'}}, 
		       {url: cgiRoot + 'edit.cgi?table=reflexes'}, 
		       {}, 
		       {});
	    $('#cogsets').jqGrid('filterToolbar', {autosearch: true, groupOn: 'AND'});
	    $("#cogsets-pager_right").empty();
	    
	    return cogsets;
	    
	};
        
        // Create the reflexes table (the table that allows access to the synchronic lexicons).
	var initReflexes = function(langnames) {
	    
	    var langFormat = function(s) { return langnames[s]; };
	    var langUnformat = function(s) { 
		$.each(langnames, function(k, v) {
		    var langid;
		    if (v == s) { langid = k; }
		    return langid;
		});
	    };

	    var reflexes = $("#reflexes").jqGrid({
		jsonReader : { repeatitems: false, id: "refid" },
		url: cgiRoot + 'query.cgi?qtype=reflexes',
		editurl: cgiRoot + 'edit.cgi',
		datatype: 'json',
		mtype: 'GET',
		height: 500,
		width: 500,
		colNames: ["ID", "Cognate Morph Map", "Form", "Gloss", "Language"],
		colModel: [
                    { name: 'refid', index:'refid', width:50, hidden: false, search: false },
		    { name: 'cogmorph', index:'cogmorph', width:50, hidden: true, search: false },
		    { name: 'form', index:'form', width:75, align: 'left', editable: true, editoptions: {size: 40},
		      formatter:reflexFormat, unformat:reflexUnformat },
		    { name: 'gloss', index:'gloss', width:100, align: 'left', editable: true, editoptions: {size: 40},
		      formatter:glossFormat, unformat:generalUnformat },
		    { name: 'langid', index:'langid', width:50, align: 'left', editable: true, edittype: "select", 
		      editoptions: {size: 40, value:langnames},
		      stype: "select", searchoptions: {value: ":All;" + objToSelectString(langnames)},
		      formatter:langFormat, unformat:langUnformat }
		],
		page: 1,
		pager: '#reflexes-pager',
		rowNum: 100,
		rowList: [25, 50, 100, 200],
		sortname: 'refid',
		sortorder: 'asc',
		viewrecords: true,
		caption: 'Reflexes',
		onSelectRow: function (refid, status) { 
		    $("body").data("refid", refid);
		    console.log($("body").data());
		},
		gridComplete: function () {
		    $(".tagged").click(function(obj) {
			var cogsetid = obj.target.title;
			$("body").data("cogsetid", cogsetid);
			updateCogSet(cogsetid);
		    });
		}
	    }).navGrid('#reflexes-pager',{view: false, search: false}, 
		       {editData: {table: 'reflexes'}}, 
		       {editData: {table: 'reflexes'}}, 
		       {url: cgiRoot + 'edit.cgi?table=reflexes'}, 
		       {}, 
		       {});
	    $('#reflexes').jqGrid('filterToolbar', {autosearch: true, groupOn: 'AND'});
	    $("#reflexes-pager_right").empty();

	    return reflexes;

	};

        /* ALL FUNCTIONS DEFINED: LET THE ACTION BEGIN... */
        
        $("body").data({cogsetid: "0", refid: "0"});

	// Set keybindings here.

	$(document).bind('keydown', 'Ctrl+a', function() {
	    var data = { oper:"addtoset",
			 cogsetid: $("body").data("cogsetid"), 
			 refid: $("body").data("refid"),
			 morphind: "0"};
	    console.log(data);
	    $.ajax({ url: cgiRoot + "edit.cgi", 
		     data: data,
		     type: "POST",
		     success: function() { updateCogSet( $("body").data("cogsetid") ); }
		   });
	});

        var winHeight = $(window).height();
        var winWidth = $(window).width();

        var authDialog = $("#auth-dialog").dialog({
            autoOpen: true,
            closeOnEscape: false,
            modal: true,
            overlay: {background: "black", opacity: 1},
            title: "Login",
            buttons: {
                "Ok" : function() {
                    var username = $("#username").val();
                    var password = $("#password").val();
                    var shaObj = new jsSHA(password);
                    var secret = shaObj.getHash("SHA-512", "HEX");
                    $.ajax({ url: cgiRoot + "query.cgi",
                             data: {
                                 oper: "auth",
                                 username: username,
                                 secret: secret
                             },
                             type: "POST",
                             async: false,
                             success: function() {
                                 $(this).dialog("close");
                             }
                           });
                    
                }
            }
        });
        
        // Create the three major user-interface components.
	var cogsets = initCogSets();
        cogsets.setGridHeight(winHeight * 0.80);
	var cogset = updateCogSet( $("body").data("cogsetid") );
	$.getJSON( cgiRoot + "query.cgi?qtype=langnames", 
		   function (langnames) {
                       var reflexes = initReflexes(langnames);
                       reflexes.setGridHeight(winHeight * 0.80);
                       // authDialog.show();
                       // authDialog.dialog("open");
                   } );

    });

var objToSelectString = function(obj) {
    if (obj) {
	pairList = [];
	$.each(obj, function(i,v) { 
	    pairList.push(i + ":" + v);
	});
	return pairList.join(";");
    } else {
	return "";
    }
};