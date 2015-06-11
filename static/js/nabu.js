var nabuApp = angular.module('nabuApp', ['nabuAlerts', 'nabuDev'])

    .controller('unitsCtrl', ['$scope', '$http', 'alerts', function($scope, $http, alerts) {
	$scope.refresh = function () {
	    $http.get('/units.json').success(function(data) { $scope.shellList = data; });
	};

	alerts.makeAvailable($scope);

	$scope.submit = function() {
	    $http.post('/addunit.json', '', {params: {"URI": $scope.manifestUri}})
		.success(function(data) {
		    alerts.add(data);
		    $scope.refresh();
		})
		.error(function() {
		    $scope.alerts.add({type: "danger", message: "Error sending URI."});
		});
	};

	$scope.refresh();
    }]);
