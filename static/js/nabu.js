var nabuApp = angular.module('nabuApp', ['nabuAlerts', 'nabuDev'])

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
	    $http.get('/chart.json', {params: {"OID": $scope.chartOid}})
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

	$scope.glyphOpacity = function(glyph) {
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
    });
