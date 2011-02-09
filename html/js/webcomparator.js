/* WHEN DOCUMENT IS READY, EXECUTE */

$(document).ready(
    function () {

        /* GLOBAL SITE SETTINGS */

        var cgiRoot = '';
        
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

        var removeReflexesFromCogset = function() {
            var rem = [];
            var refids = [];
            var prefid = $("body").data("prefid");
            var morphinds = [];
            $(".selected").each( function(i) {
                var data = $(this).data();                
                rem.push( { refid: data.refid,
                            morphind: data.morphind } );
                refids.push(data.refid);
                morphinds.push(data.morphind);
            });
            $.ajax({
                url: cgiRoot + "edit.cgi",
                data: {oper: "removegroupfromset",
                       prefid: prefid,
                       refids: refids.join(","),
                       morphinds: morphinds.join(",")
                      },
                type: "POST",
                success : function () {
                    updateCogSet( $("body").data("prefid") );
                    $("body").data("lastremoved", rem);
                    $("#reflexes")[0].triggerToolbar();
                }
            });
        };
        
        var addReflexesToCogset = function() {
	    var refids = $("#reflexes").jqGrid("getGridParam", "selarrrow").join(",");
            if (refids) {
	        var data = { oper:"addgrouptoset",
			     prefid: $("body").data("prefid"), 
			     refids: refids,
			     plangid: $("#plangid").val(),
			     protoform: $("#cogset-protoform").data("form"),
			     protogloss: $("#cogset-protoform").data("gloss")
		           };
	        $.ajax({ url: cgiRoot + "edit.cgi",
		         data: data,
		         type: "POST",
		         success: function() {
                             $("#reflexes").resetSelection();
			     $("#reflexes")[0].triggerToolbar();
                             updateCogSet( $("body").data("prefid") );
                         }
		       });
            }
        };

        var pasteReflexesToCogset = function() {
            var refids = [];
            var morphinds = [];
            $.each($("body").data("lastremoved"), function(k, v) {
                refids.push(v.refid);
                morphinds.push(v.morphind);
            });
            var data = { oper:"pastegrouptoset",
                         prefid: $("#cogset-box").data("prefid"),
                         plangid: $("#plangid").val(),
                         refids: refids.join(","),
                         morphinds: morphinds.join(",")
                       };
            $.ajax({ url: cgiRoot + "edit.cgi",
		     data: data,
		     type: "POST",
		     success: function() {
                         $("#reflexes").resetSelection().triggerToolbar();
                         updateCogSet( $("body").data("prefid") );
                     }
            });
        };
        
        /* MORPH-PICKER */        
        
        // Instantiate the morph-picker dialog
	var morphDialog =
            $("#morph-picker-dialog").dialog({
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
					prefid: data.prefid, 
					morphind: data.morphind}, 
				 type: "POST",
				 success: function() { updateCogSet(data.prefid); }
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
                                   prefid: data.prefid,
                                   refid: data.refid
                                  },
                            type: "POST",
                            success: function () {
                                updateCogSet( data.prefid );
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

	var updateCogSetInfo = function(protoForm, protoGloss) {
	    $("#cogset-protoform").data({form: protoForm, gloss: protoGloss});
	    $("#cogset-protoform").empty().append("*" + protoForm);
	    $("#cogset-protogloss").empty().append("‘" + protoGloss + "’");
	};

	// Refreshes the display of the current cognate set.
	var updateCogSet = function(prefid) {

	    var plangid = $("#plangid").val();
	    
            // Set the value of prefid in two useful locations.
	    $("body").data("prefid", prefid);
	    $("#cogset-box").data("prefid", prefid);
            
            // URL for getting cogset JSON.
            var url = cgiRoot + "query.cgi?qtype=cogset&prefid=" + prefid + "&plangid=" + plangid;
                
	    var protoForm = $("#cogsets").getCell(prefid, "form");
	    var protoGloss = $("#cogsets").getCell(prefid, "gloss");

	    if (!protoForm && !protoGloss) {
		var refid = prefid;
		$.ajax({
		    url: cgiRoot + "query.cgi",
		    data: {
			qtype: "single",
			refid: refid
		    },
		    dataType: "json",
		    type: "GET",
		    success: function (data) {
			console.log("data="+data);
			updateCogSetInfo(data.form, data.gloss);
		    }
		});
	    } else {
		updateCogSetInfo(protoForm, protoGloss);
	    }

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
					     prefid: prefid,
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
                            thisForm.click( function() {
                                thisForm.toggleClass("selected ui-state-highlight");
                            });
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

                setDimensions();
                
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
	var initCogSets = function(plangid) {

	    var cogsets = $("#cogsets").jqGrid({
		jsonReader : { repeatitems: false, id: "refid" },
		url: cgiRoot + 'query.cgi?qtype=cogsets&langid=' + plangid,
		editurl: cgiRoot + 'edit.cgi',
		//editData: {table: 'cogsets', langid: function() {return $("#plangid").val(); } },
		datatype: 'json',
		mtype: 'GET',
		height: "100%",
		width: 350,
		colNames: ["Set ID", "Proto-form", "Gloss", "Num"],
		colModel: [
                    { name:'refid', index:'refid', width:50, hidden: true, search: false },
		    { name: 'form', index:'form', width:50, align: 'left', editable: true, editoptions: {size: 40},
		      formatter:protoformFormat, unformat:protoformUnformat },
		    { name: 'gloss', index:'gloss', width:100, align: 'left', editable: true, editoptions: {size: 40},
		      formatter:glossFormat, unformat:generalUnformat },
		    { name: "numref", index:'numref', width:25, hidden: false, search: true }
		],
		page: 1,
		pager: '#cogsets-pager',
		rowNum: 100,
		rowList: [25, 50, 100, 200],
		sortname: 'refid',
		sortorder: 'ASC',
		viewrecords: true,
		caption: 'Proto-Forms',
		onSelectRow: function(prefid) { 
		    updateCogSet(prefid);
		    $("body").data("prefid", prefid);
		}
	    }).navGrid('#cogsets-pager', {view: false, search: false}, 
		       {editData: {table: 'cogsets', langid: function() {return $("#plangid").val(); } }}, 
		       {editData: {table: 'cogsets', langid: function() {return $("#plangid").val(); } }}, 
		       {url: cgiRoot + 'edit.cgi?table=reflexes&langid=' + plangid}, 
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
		url: cgiRoot + 'query.cgi?qtype=reflexes&plangid=' + $("#plangid").val(),
		editurl: cgiRoot + 'edit.cgi',
		data: { plangid: function() {return $("#plangid").val(); } },
		editData: { plangid: function() {return $("#plangid").val(); } },
		datatype: 'json',
		mtype: 'GET',
		height: "100%",
		width: 400,
		colNames: ["ID", "Cognate Morph Map", "Form", "Gloss", "Language", "Group"],
		colModel: [
                    { name: 'refid', index:'refid', width:50, hidden: true, search: false },
		    { name: 'cogmorph', index:'cogmorph', width:50, hidden: true, search: false },
		    { name: 'form', index:'form', width:75, align: 'left', editable: true, editoptions: {size: 40},
		      formatter:reflexFormat, unformat:reflexUnformat },
		    { name: 'gloss', index:'gloss', width:100, align: 'left', editable: true, editoptions: {size: 40},
		      formatter:glossFormat, unformat:generalUnformat },
		    { name: 'langid', index:'langnames.langid', width:50, align: 'left', editable: true, edittype: "select", 
		      editoptions: {size: 40, value:langnames},
		      stype: "select", searchoptions: {value: ":All;" + objToSelectString(langnames)},
		      formatter:langFormat, unformat:langUnformat },
		    { name: 'langgrp', index:'langgrp', width:50, align: 'left'}
		],
		page: 1,
		pager: '#reflexes-pager',
		rowNum: 100,
		rowList: [25, 50, 100, 200],
		sortname: 'refid',
		sortorder: 'ASC',
		viewrecords: true,
		multiselect: true,
		caption: 'Reflexes',
		onSelectRow: function (refid, status) { 
		        $("body").data("refid", refid);
		},
		gridComplete: function () {
		    $(".tagged").click(function(obj) {
			var prefid = obj.target.title;
			$("body").data("prefid", prefid);
			updateCogSet(prefid);
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
        
        $("body").data({prefid: "0", refid: "0"});

	// Set keybindings here.

	$(document).bind('keydown', 'Ctrl+a', function() { addReflexesToCogset(); });            
        
        var authDialog = $("#auth-dialog").dialog({
            autoOpen: false,
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
        
	    var protoLangSelector = function(plangnames) {
	        $.each(plangnames, function(k, v) {
		    $("#plangid").append("<option value='" + k + "'>" + v + "</option>");
	        });

	        $("#plangid").change(function() {
		    var plangid = $("#plangid").val();
		    var cogsetUrl = cgiRoot + 'query.cgi?qtype=cogsets&langid=' + plangid;
		    $("#cogsets").jqGrid().setGridParam({
		        url : cogsetUrl, 
		        editData: {langid: plangid}
		    }).trigger("reloadGrid");
		    var reflexesUrl = cgiRoot + 'query.cgi?qtype=reflexes&plangid=' + plangid;
		    $("#reflexes").jqGrid().setGridParam({
		        url : reflexesUrl, 
		        editData: {plangid: plangid}
		    }).trigger("reloadGrid");
		    updateCogSet(0);
	        });

	        $("#plangid").val("18");
	        return $("#plangid").val(); // default plangid
	    };

        var setDimensions = function() {
            var winHeight = $(window).height();
            var winWidth = $(window).width();
            
            $("#controls").height(winHeight * 0.9).width(winWidth * 0.9);
	    $("#cogsets").setGridHeight(winHeight * 0.80);
            $("#reflexes").setGridHeight(winHeight * 0.80);

            var availableWidth = winWidth - 60;
            $("#cogsets").setGridWidth(availableWidth * 0.3);
            $("#reflexes").setGridWidth(availableWidth * 0.4);

            $("#cogset-box")
                .css("max-height", (winHeight * 0.80 + 50) + "px")
                .width(availableWidth * 0.3);
            $("#cogset-table").width(availableWidth * 0.3);
            $("#cogset-table td.langname").css("max-width", "150px");
            $("#cogset-table td.reflex").width((availableWidth * 0.3) - 150);
        };
        
        // Create the three major user-interface components.
	$.getJSON( cgiRoot + "query.cgi?qtype=plangnames",
		   function(plangnames) {
		       var plangid = protoLangSelector(plangnames);
                       $.getJSON( cgiRoot + "query.cgi?qtype=langnames", 
		                  function (langnames) {
				      $("body").data("langnames", langnames);
				      var cogset = updateCogSet( $("body").data("prefid") );
                                      var reflexes = initReflexes(langnames);
				      var cogsets = initCogSets(plangid);
                                      $("#cogset-add").button().css("width", "23%");
                                      $("#cogset-remove").button().css("width", "23%");
                                      $("#cogset-paste").button().css("width", "23%");
                                      $("#cogset-select").button().css("width", "23%");
                                      $("#cogset-add").click( function(){ addReflexesToCogset(); });
                                      $("#cogset-remove").click( function(){ removeReflexesFromCogset(); });
                                      $("#cogset-paste").click( function(){ pasteReflexesToCogset(); });
				      $("#cogset-select").click( function() { 
					  if ($("div.ref.selected").size() == $("div.ref").size()) {
					      $("div.ref").removeClass("selected ui-state-highlight");
					  } else {
					      $("div.ref").addClass("selected ui-state-highlight");
					  }

				      });
                                      setDimensions();
                                  } );
                       
		   }
		 );

        $(window).resize( function() {
            console.log("Window resized.");
            setDimensions();
        });
    });

var getLangId = function(s) { 
    var langid;
    $.each($("body").data("langnames"), function(k, v) {
	if (v == s) { langid = k; }
    });
    return langid;
};

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