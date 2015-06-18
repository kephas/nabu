var nabuApp = angular.module('nabuApp', ['ngAnimate', 'nabuAlerts', 'nabuDev'])

    .controller('unitsCtrl', function($scope, $http, alerts) {
	$scope.refresh = function () {
	    $http.get('/units.json').success(function(data) { $scope.shellList = data; });
	};

	alerts.makeAvailable($scope);

	$scope.submit = function() {
	    $http.post('/addunit.json', '{}', {params: {"URI": $scope.manifestUri}})
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

    .controller('chartCtrl', function($scope, $rootScope, $http) {
	$scope.refresh = function() {
	    $http.get('/chart.json', {params: {"OID": $scope.chartOid, "PUBLIC": $scope.public}})
		.success(function(data) {
		    $scope.chart = data;
		    $rootScope.name = $scope.chart.name;
		})
		.error(function() {
		    $rootScope.name = "Chart not found";
		});
	};
    })

    .controller('chartEditCtrl', function($scope, $rootScope, $http, alerts) {
	$scope.refresh = function() {
	    $http.get('/chart.json', {params: {"OID": $scope.chartOid}})
		.success(function(data) {
		    $scope.chart = data;
		    $rootScope.name = $scope.chart.name;
		})
		.error(function() {
		    $rootScope.name = "Chart not found";
		});
	};

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

	$scope.submit = function() {
	    $http.post('/chart.json', $scope.chart, {params: {"OID": $scope.chartOid}})
		.success(function() {
		    alerts.add({type: "success", message: "Modifications saved."});
		})
		.error(function() {
		    alerts.add({type: "danger", message: "An error occurred while trying to save modifications."});
		});
	};

	$scope.publish = function() {
	    $http.post('/pub-chart.json', {oid: $scope.chart.oid})
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
    })

    .directive('nabuGlyph', function() {
	return {
	    restrict: 'E',
	    templateUrl: '/static/ng/glyph.html'
	};
    })
    .directive('nabuGlyphEdit', function() {
	return {
	    restrict: 'E',
	    templateUrl: '/static/ng/glyph-editor.html'
	};
    })

    .controller('chartCompareCtrl', function($scope, $http, $timeout, alerts) {
	$scope.download = function() {
	    $http.get('/charts.json?' + $scope.oidsParams)
		.success(function(data) {
		    $scope.comparison = data;
		})
		.error(function() {
		    alerts.add({type: "danger", message: "An error occurred while trying to retrieve the charts."});
		});
	};

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
