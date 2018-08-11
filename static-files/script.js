$(":header").click(function(event){
     //alert($(event.currentTarget).siblings("form").innerHTML);
     //$(event.currentTarget).siblings("form").toggleClass("hidden");
     $( ".bordered-rightcol form" ).toggleClass("hidden");
   });

$(document).on('click', '#problem-button', function(){ $.ajax({
     headers: {
     'Accept': 'application/json',
     'Content-Type': 'application/json'
     },
     url: "/problem",
     type: "POST",
     data: createJsonObjectWithProblemArray("#problem"),
     success: function( result ) {
      $( "#problem-spec" ).html("<p>" + result.description+"</p>");
      MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
     },
     error: function( result ) {
        str = JSON.stringify(result);
        alert(str);
     }
     })});

 $(document).on('click', '#expansions-button', function(){ alert(createJsonObject("#expansions")); $.ajax({
   headers: {
     'Accept': 'application/json',
     'Content-Type': 'application/json'
   }, type: "POST",
   url: "/expansions",
   data: createJsonObject("#expansions"),
   dataType: "json",
   success: function( result ) {
      $( "#expansions-spec" ).html(result.expansionFrom+" | "+result.expansionTo);
    },
   error: function( result ) {
      str = JSON.stringify(result);
      alert('error: ' + str);
   }
   })});

 $(document).on('click', '#rewrites-button', function(){ $.ajax({
    headers: {
     'Accept': 'application/json',
     'Content-Type': 'application/json'
    }, url: "/rewrites",
    type: "POST",
    data: createJsonObject("#rewrites"),
    dataType: "json",
    success: function( result ) {
      console.log(result); $( "#rewrites-spec" ).html(result.rewriteFrom+" | "+result.rewriteTo);
    },
    error: function( result ) {
      str = JSON.stringify(result);
      alert(str);
    }
    })});

 $(document).on('click', '#library-button', function(){ $.ajax({
    headers: {
    'Accept': 'application/json',
    'Content-Type': 'application/json'
    },
    type: "POST",
    url: "/library",
    data: createJsonObjectWithArray("#library"),
    dataType: "json",
    success: function( result ) {
     console.log(result); $( "#library-spec" ).html(result.premises+" | "+result.conclusion);
    },
    error: function( result ) {
        str = JSON.stringify(result);
        alert(str);
    }
    })});

 function createJsonObject(formName){
     var arr = $(formName).serializeArray();
     var json = {};
     jQuery.each(arr, function() {
        json[this.name] = this.value || '';
     });
     var json_string = JSON.stringify(json);
     json_string.replace(/(\\s*?{\\s*?|\\s*?,\\s*?)(['"])?([a-zA-Z0-9]+)(['"])?:/g, '$1"$3":');
     return json_string;
 }

 function createJsonObjectWithArray(formName){
     var arr = $(formName).serializeArray();
     var json = {};
     jQuery.each(arr, function() {
        json[this.name] = this.value || '';
     });
     var json_string = JSON.stringify(json);
     json_string.replace(/(\\s*?{\\s*?|\\s*?,\\s*?)(['"])?([a-zA-Z0-9]+)(['"])?:/g, '$1"$3":');
     eval('var jsonO = new Object(' + json_string + ')'); jsonO.libraryPremises = jsonO.libraryPremises.split("@");
     var json_edit_string = JSON.stringify(jsonO);
     return json_edit_string;
 }

 function createJsonObjectWithProblemArray(formName){
     var arr = $(formName).serializeArray();
     var json = {};
     jQuery.each(arr, function() {
        json[this.name] = this.value || '';
     });
     var json_string = JSON.stringify(json);
     json_string.replace(/(\\s*?{\\s*?|\\s*?,\\s*?)(['"])?([a-zA-Z0-9]+)(['"])?:/g, '$1"$3":');
     eval('var jsonO2 = new Object(' + json_string + ')'); console.log(jsonO2); jsonO2.problemPremises = jsonO2.problemPremises.split("@");
     var json_edit_string = JSON.stringify(jsonO2);
     return json_edit_string;
 }