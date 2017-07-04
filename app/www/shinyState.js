$(function() {
  
  $(document).on({

    'shiny:busy': function(event) {
      
      BusyTimer = setTimeout(function(){
        
      d3.select("#BusyDIV").transition().duration(250).style({"background-color":"white","opacity":"0.5"});
                
      }, 350);

    },
    'shiny:idle': function(event) {
      
      clearTimeout(BusyTimer);
            
      d3.select("#BusyDIV").transition().duration(250).style({"background-color":"transparent","opacity":"1"});
    }
  });
});
