var nabuAlerts = angular.module('nabuAlerts', [])

    .factory('alerts', function(){
	return {
	    alerts: [],
	    makeAvailable: function(scope) {
		scope.alerts = this;
	    },
	    add: function(alert) {
		alert.id = Date.now();
		this.alerts.push(alert);
	    },
	    dismiss: function(id) {
		var alerts = this.alerts;
		var max = alerts.length;
		var rec = function(pos) {
		    if(pos < max) {
			if(alerts[pos].id == id) {
			    alerts.splice(pos, 1);
			} else {
			    rec(pos + 1);
			}
		    }
		};
		rec(0);
	    }
	};
    })

    .directive('nabuAlerts', function() {
	return {
	    restrict: 'E',
	    templateUrl: '/static/ng/alerts.html'
	}
    });
