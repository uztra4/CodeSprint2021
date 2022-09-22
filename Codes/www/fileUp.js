/*jshint loopfunc:true */ // git rid of warning
var datasets = {};
var dragOver = function(e) { e.preventDefault(); };

var dropData = function(e) {
  e.preventDefault();
  handleDrop(e.dataTransfer.files);
};
var dropcount=0;

var removeFiles = function(e){
    txt = "Drop Area "+dropcount;
    jQuery('#drop-area').html(txt);
    datasets = {};
    Shiny.onInputChange(ns("datafile"), datasets);
};
var handleDrop = function(files) {
  for (var i = 0; i<files.length; i++) {
    f = files[i];
    var reader = new FileReader();

    reader.onload = (function(file) {
      return function(e) {
        datasets[file.name.toLowerCase()+'|'+dropcount] = e.target.result;
        Shiny.onInputChange("datafile", datasets);
        var div = document.createElement("div");
        var src = "https://cdn0.iconfinder.com/data/icons/office/512/e42-512.png";
        div.id = "datasets";
        div.innerHTML = [
          "<img class='thumb' src='", src, "' title='", encodeURI(file.name),
          "'/>", "<br>", file.name, "<br>"].join('');
        drpel = document.getElementById("drop-area");
        drpel.appendChild(div);
        drpel.childNodes[0] = "Drop Area "+dropcount;
      };
    })(f);
    reader.readAsText(f);
    dropcount++;
  }
};