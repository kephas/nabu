var nabuApp = angular.module('nabuApp', ['ngAnimate', 'ipCookie', 'ngResource', 'ngRoute', 'nabuAlerts', 'nabuDev'])

    .config(function($locationProvider, $routeProvider) {
	$locationProvider.html5Mode(true);//.hashPrefix('!');
	$routeProvider
	    .when('/user/:uid/units',
		  {
		      templateUrl: '/ng/units',
		      controller: 'unitsCtrl'
		  })
	    .when('/user/:uid/charts',
		  {
		      templateUrl: '/ng/charts',
		      controller: 'chartsCtrl'
		  })
	    .when('/user/:uid/chart',
		  {
		      templateUrl: '/ng/chart',
		      controller: 'chartCtrl'
		  })
	    .when('/user/:uid/edit-chart',
		  {
		      templateUrl: '/ng/edit-chart',
		      controller: 'chartEditCtrl'
		  })
	    .when('/user/:uid/compare',
		  {
		      templateUrl: '/ng/compare',
		      controller: 'chartsCompareCtrl'
		  })
	    .when('/pub/chart/:oid',
		  {
		      templateUrl: '/ng/chart',
		      controller: 'chartCtrl'
		  });
    })

    .controller('userCtrl', function($scope, $routeParams, ipCookie) {
	$scope.remembered = false;
	
	$scope.$on('$routeChangeSuccess', function() {
	    if (ipCookie('uid')) {
		$scope.remembered = true;
	    }

	    // if user X is remembered and we go to a private page of user
	    // Y, something's wrong and we forget X by precaution
	    if ($routeParams.uid.length > 0 && ipCookie('uid') != $routeParams.uid) {
		$scope.forget();
	    }
	});

	$scope.remember = function() {
	    ipCookie('uid', $routeParams.uid, {path: '/'});
	    $scope.remembered = true;
	};

	$scope.forget = function() {
	    ipCookie('uid', false, {path: '/'});
	    $scope.remembered = false;
	};
    })

/*
  Units
*/

    .controller('unitsCtrl', function($scope, $resource, $routeParams, alerts) {
	var apiEndpoint = '/api/user/' + $routeParams.uid + '/units';
	var Units = $resource('/api/user/:uid/units', $routeParams);

	$scope.refresh = function () {
	    $scope.shellList = Units.query();
	};

	alerts.makeAvailable($scope);

	$scope.submit = function() {
	    $http.post(apiEndpoint, '{}', {params: {"URI": $scope.manifestUri}})
		.success(function(data) {
		    alerts.add(data);
		    $scope.refresh();
		})
		.error(function() {
		    $scope.alerts.add({type: "danger", message: "Error sending URI."});
		});
	};

	$scope.refresh();
    })

/*
  Charts
*/

    .controller('chartsCtrl', function($scope, $resource, $routeParams) {
	var Charts;

	$scope.initialize = function() {
	    Charts = $resource('/api/user/:uid/charts', $routeParams);
	    $scope.charts = Charts.query();
	};

	$scope.uid = $routeParams.uid;
	$scope.action = "/user/" + $routeParams.uid + "/compare";
    })

/*
  Chart view
*/

    .controller('chartCtrl', function($scope, $resource, $routeParams) {
	if ( $routeParams.uid ) {
	    $scope.chart = $resource('/api/user/:uid/charts/:oid', $routeParams).get();
	} else {
	    $scope.chart = $resource('/api/public/charts/:oid', $routeParams).get();
	}
	$scope.uid = $routeParams.uid;

	$scope.glyphCssDimensions = function(chart, glyph) {
	    if (chart.scale == 100) {
		return "";
	    } else {
		return "height:" + Math.round(glyph.height * chart.scale / 100) + "px;"
		    + "width:" + Math.round(glyph.width * chart.scale / 100) + "px";
	    }
	};
    })
    .directive('nabuGlyph', function() {
	return {
	    restrict: 'E',
	    templateUrl: '/static/ng/glyph.html'
	};
    })


/*
  Chart edition
*/

    .controller('chartEditCtrl', function($scope, $http, $resource, $routeParams, alerts) {
	var Chart = $resource('/api/user/:uid/charts/:oid', $routeParams);
	$scope.chart_res = Chart;

	$scope.chart = Chart.get(function() {
	    if ($scope.chart.scale != 100) {
		$scope.scaling = true;
	    }
	});
	$scope.uid = $routeParams.uid;

	alerts.makeAvailable($scope);

	var focusedGlyph;
	$scope.focusGlyph = function(id) {
	    if (focusedGlyph == id) {
		focusedGlyph = null;
	    } else {
		focusedGlyph = id;
	    }
	}
	$scope.isFocused = function(id) {
	    return id == focusedGlyph;
	}

	$scope.toggleGlyph = function(glyph) {
	    glyph.active = !glyph.active;
	    focusedGlyph = null;
	};

	$scope.glyphCssOpacity = function(glyph) {
	    var alpha = glyph.active ? 1 : .5;
	    return "opacity:" + alpha +
		";filter:alpha(opacity=" + alpha * 100 + ")";
	};

	$scope.glyphCssDimensions = function(chart, glyph) {
	    if (chart.scale == 100) {
		return "";
	    } else {
		return "height:" + Math.round(glyph.height * chart.scale / 100) + "px;"
		    + "width:" + Math.round(glyph.width * chart.scale / 100) + "px";
	    }
	};

	$scope.submit = function() {
	    Chart.save({}, $scope.chart, function() {
		alerts.add({type: "success", message: "Modifications saved."});
	    }, function() {
		alerts.add({type: "danger", message: "An error occurred while trying to save modifications."});
	    });
	};

	$scope.publish = function() {
	    $http.post('/api/user/' + $routeParams.uid +
		       '/charts/' + $routeParams.oid + '/publish')
		.success(function(data) {
		    $scope.chart.publicOid = data.publicOid;
		});
	};

	$scope.unpublish = function() {
	    $http.post('/unpub-chart.json', {oid: $scope.chart.oid})
		.success(function() {
		    $scope.chart.publicOid = false;
		});
	};

	/*
	  Scaling
	*/

	$scope.dimensions = [];
	$scope.scaling = false;

	function storeDimensions() {
	    $scope.dimensions.forEach(function(entry) {
		entry.glyph.height = entry.height;
		entry.glyph.width = entry.width;
	    });
	};

	$scope.activateScaling = function() {
	    $scope.scaling = true;

	    // only store them here because if scaling was already on,
	    // images wouldn't have their original size
	    storeDimensions();
	};
    })

    .directive('nabuGlyphEdit', function() {
	return {
	    restrict: 'E',
	    templateUrl: '/static/ng/glyph-editor.html'
	};
    })
    .directive('dimensionMonitor', function() {
	return {
	    restrict: 'A',
	    link: function(scope, element, attr) {
		element.on('load', function() {
		    scope.dimensions.push({
			glyph: scope.glyph,
			height: element.height(),
			width: element.width()
		    });
		});
	    }
	};
    })

    .controller('chartsCompareCtrl', function($scope, $resource, $routeParams, $location, $timeout, alerts) {
	$scope.comparison = $resource("/api/user/:uid/comparative-chart", $routeParams)
	    .get($location.search(), function(){}, function() {
		alerts.add({type: "danger", message: "An error occurred while trying to retrieve the charts."});
	    });

	$scope.move = function(index, shift) {
	    var removed = $scope.comparison.charts.splice(index, 1);
	    $timeout(function() {
		$scope.comparison.charts.splice(index + shift, 0, removed[0]);
	    }, 500);
	};
    })

    .directive('chartReorder', function() {
	return {
	    restrict: 'E',
	    templateUrl: '/static/ng/chart-reorder.html'
	};
    });
