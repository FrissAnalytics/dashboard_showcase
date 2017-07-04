// getDimensions
Shiny.addCustomMessageHandler('browserInfo', function(id){

  // browser version
  navigator.sayswho= (function(){
      var N= navigator.appName, ua= navigator.userAgent, tem;
      var M= ua.match(/(opera|chrome|safari|firefox|msie)\/?\s*(\.?\d+(\.\d+)*)/i);
      if(M && (tem= ua.match(/version\/([\.\d]+)/i))!= null) M[2]= tem[1];
      M= M? [M[1], M[2]]: [N, navigator.appVersion,'-?'];
      return M;
  })();

  var browser_version	= navigator.sayswho;

  // window dimensions prior to window resize
  var dimInfo = { height: $(window).height(), width : $(window).width(), browser_version: browser_version	};

  console.log("navigator");
  console.log(navigator);

  Shiny.onInputChange(id + "dim", dimInfo);

  // window dimensions after window resize
  $(window).resize(function(){

  var dimInfo = { height: $(window).height(), width : $(window).width(), browser_version: browser_version	};

    Shiny.onInputChange(id + "dim", dimInfo);
  });

});