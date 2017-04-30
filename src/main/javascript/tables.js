$(function () {
	var tabContainer = $('#tabsContainer');
	// initialize
	tabContainer.masonry({
	  columnWidth: 200,
	  itemSelector: '.tab'
	});
	
	var isActive = function(status){
		return $.inArray(status,[1,2]) > -1
	}
	
	var activateTableButtons = function(){
		
		$('.show-carts-button').click(function(e){
			$(this).closest('.tab').find('.cart').each(function(index,element){
				if ( !isActive($(element).data('status')) ){
					$(element).fadeIn(400,function(){
						 tabContainer.masonry('layout');
					});
				}
			});
		});
		$('.hide-carts-button').click(function(e){
			$(this).closest('.tab').find('.cart').each(function(index,element){
				if ( !isActive($(element).data('status')) ){
					$(element).fadeOut(400,function(){
						 tabContainer.masonry('layout');
					});
				}
			});
		});
		
		setTimeout( function(){$('.hide-carts-button').trigger( "click" )},1000);
	}
	activateTableButtons();
	
	window.tabAdded = function(tabId){
		activateTableButtons();
	}
	
	window.cartUpdated = function(cartId){
		var cart = $('#'+ cartId);
		cart.find('a').timeago();
		if ( $('.hide-carts-button').hasClass('active') ){
			if( !isActive(cart.data('status')) ){
				cart.fadeOut(400,function(){
					 tabContainer.masonry('layout');
				});
			}
		}else{
			tabContainer.masonry('layout');
		}
	};
});