  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-89533180-1', 'auto');
  ga('send', 'pageview');
	
	 $(document).ready(function() {
        $(document.getElementById('get_excel_file')).click(function(){
           var selected_value = []; // initialize empty array 
		   $("input:checkbox[name=selected_models]:checked").each(function() {    
        selected_value.push($(this).val());
		//alert($(this).val()); 
		ga('send', 'event', 'widget', 'selected models', $(this).val()); 	
    });
	//alert(selected_value);  
     
  });
   });