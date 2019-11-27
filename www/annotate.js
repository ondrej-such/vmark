




shinyjs.plot1_draw = function (params) {
  alert(-1);
  var canvas = document.getElementById('spectrum1');
  //canvas.style.visibility= "visible";
  //Always check for properties and methods, to make sure your code   doesn't break in other browsers.
  alert("alert 1");
  
   var defaultParams = {
    x : null,
    ymin : 0,
    ymax: 1400
  };
  
  alert("3");
  
  params = shinyjs.getParams(params, defaultParams);
  alert("4");
  
  if (canvas.getContext) 
  {

    alert("alert 2");
    alert(params.x);
    alert(params.ymin);
    alert(params.ymax);
    
    
    var context = canvas.getContext('2d');
    context.clearRect(0, 0, 600, 400);
    //alert(context.fillstyle);
    //alert(context.strokestyle);
    // Reset the current path
    context.beginPath(); 
    // Staring point (10,45)
    //context.moveTo(params.x, params.ymin);
    context.moveTo(params.x, 0);
    // End point (180,47)
    context.lineTo(params.x, 1400);
    // Make the line visible
    context.stroke();
  }  
};

shinyjs.reset_selId = function(event) {
  Shiny.setInputValue("selId", -1);
};


spectrum1_click = function(event) {
  Shiny.setInputValue("x1", event.offsetX);
};

spectrum2_click = function(event) {
  Shiny.setInputValue("x2", event.offsetX);
};

spectrum1_click_old = function(event) {
  var canvas = document.getElementById('spectrum1');
  if (canvas.getContext) 
  {
    alert(7);
    var context = canvas.getContext('2d');
    
    context.clearRect(0, 0, canvas.width, 400);
    // alert(context.fillstyle);
    alert(context.strokestyle);
    // Reset the current path
    context.beginPath(); 
    // Staring point (10,45)
    context.moveTo(event.offsetX, 0);
    // End point (180,47)
    context.lineTo(event.offsetX, canvas.height);
    // Make the line visible
    context.stroke();
    Shiny.setInputValue("x1", event.offsetX);
  }
  //var allSpectrum = document.getElementById('allSpectrum');
  //allSpectrum.onclick(event);
};

shinyjs.erase_detail = function(params) {
  var canvas = document.getElementById('spectrum2');
  var context = canvas.getContext('2d');
  if (context) 
  {
      context.clearRect(0, 0, canvas.width, 400);
  }
  
  //document.getElementById('phoneStart').textContent = ''
  //document.getElementById('phoneEnd').textContent = ''
};

shinyjs.erase_spectrum2_ann = function(params) {
  var canvas = document.getElementById('spectrum2_ann');
  var context = canvas.getContext('2d');
  if (context) 
  {
      context.clearRect(0, 0, canvas.width, 400);
  }
};


shinyjs.draw_label = function(params) {
  var defaultParams = {
    vocal : 'a',
    start : 0,
    finish : 0
  };
  
  //alert("3");
  
  params = shinyjs.getParams(params, defaultParams);
  var canvas = document.getElementById('spectrum2_ann');
  var context = canvas.getContext('2d');
  if (context) {
    context.font = "italic 15px Arial";
    context.fillText(params.vocal, (params.start + params.finish)/2, 52);
    /*
     context.beginPath(); 
    // Staring point (10,45)
    context.moveTo(params.start, 40);
    // End point (180,47)
    context.lineTo(params.start, 60);
    context.closePath();
    context.stroke();
    context.beginPath(); 
    // Staring point (10,45)
    context.moveTo(params.finish, 40);
    // End point (180,47)
    context.lineTo(params.finish, 60);
    context.closePath();
    context.stroke();
    */
  }
};

shinyjs.draw_detail = function(params) {
   var defaultParams = {
    pos1 : 0,
    end : 0
  };
  
  //alert("3");
  
  params = shinyjs.getParams(params, defaultParams);
  pos1 = params.pos1;
  
  var canvas = document.getElementById('spectrum2');
  var context = canvas.getContext('2d');

  if (context) 
  {
    d = 0.1;
    context.beginPath(); 
    // Staring point (10,45)
    context.moveTo(pos1, d * canvas.height);
    // End point (180,47)
    context.lineTo(pos1, (1 - d) * canvas.height);
    context.closePath();
    context.stroke();
    context.font = "15px Arial";
    if (params.end > 0) {
      context.fillText("koniec", pos1 - 7, 30);
      // alert("koniec");
    } else {
      context.fillText("začiatok", pos1 - 7, 30);
      // alert(začiatok)
    }
  }
};

shinyjs.write_boundary = function(params) {
    var defaultParams = {
    time : 0,
    end: 0
  };
  
  params = shinyjs.getParams(params, defaultParams);
  
  var tdElement;
  if (params.end > 0) {
    tdElement = document.getElementById("phoneEnd");
  } else {
    tdElement = document.getElementById("phoneStart");
  }
  tdElement.textContent = params.time;
};

shinyjs.write_boundaryDB = function(params) {
    var defaultParams = {
    time : 0,
    end: 0
  };
  
  params = shinyjs.getParams(params, defaultParams);
  
  var tdElement;
  if (params.end > 0) {
    tdElement = document.getElementById("phoneEndDB");
  } else {
    tdElement = document.getElementById("phoneStartDB");
  }
  tdElement.textContent = params.time;
};

shinyjs.set_x1 = function(params) {
  
  var defaultParams = {
    x1 : 0,
  };
  params = shinyjs.getParams(params, defaultParams);
  Shiny.onInputChange("x1", params.x1);
  // alert(17)
  // alert(params.x1)
};

shinyjs.spectrum1_draw2 = function(params) {
  
  var defaultParams = {
    pos1 : 0,
    pos2: 199
  };
  
  //alert("3");
  
  params = shinyjs.getParams(params, defaultParams);
  pos1 = params.pos1;
  pos2 = params.pos2;
  
  var canvas = document.getElementById('spectrum1');
  var context = canvas.getContext('2d');
  if (context) 
  {
    //alert(7);
  
    context.clearRect(0, 0, canvas.width, 400);
    
    // context.clearRect(0, 0, canvas.width, 400);
    // alert(context.fillstyle);
    //alert(context.strokestyle);
    // Reset the current path
    d = 0.1;
    //alert(8);
    //alert(pos1);
    //alert(pos2);
    
    // ctx.fillStyle = '#f00';
    context.beginPath(); 
    // Staring point (10,45)
    context.moveTo(pos1, d * canvas.height);
    // End point (180,47)
    context.lineTo(pos1, (1 - d) * canvas.height);
    
    context.lineTo(pos2, (1 - d) * canvas.height);
    
    context.lineTo(pos2, d * canvas.height);
    
    context.lineTo(pos1, d * canvas.height);
    
    context.closePath();
    context.stroke();
    // Make the line visible
    // context.stroke();
  } 
  //var allSpectrum = document.getElementById('allSpectrum');
  //allSpectrum.onclick(event);
};

draw_spec = function(id) {
  // alert("wavId set")
  Shiny.onInputChange("wavId", id);
};

shinyjs.reset_spec = function() {
  Shiny.onInputChange("wavId", -1);
  Shiny.onInputChange("x2", -1);
  Shiny.onInputChange("x1", -1);
};



// add on click handlers to a table
// https://stackoverflow.com/questions/1207939/adding-an-onclick-event-to-a-table-row
shinyjs.addRowHandlers = function () {
  var table = document.getElementById("summary").childNodes[0];
  alert("In addRowHandlers")
  var rows = table.getElementsByTagName("tr");
  for (i = 0; i < rows.length; i++) {
    var currentRow = table.rows[i];
    var createClickHandler = function(row) {
      return function() {
        var cell = row.getElementsByTagName("td")[0];
        var id = cell.innerHTML;
        alert("id:" + id);
      };
    };
    currentRow.onclick = createClickHandler(currentRow);
  }
}
/* document.addEventListener("click", spectrum1_click); */
