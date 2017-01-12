var gmail = require('googleapis').gmail('v1');

exports.getMessages = function(options) {
  return function(callback) {
    return function() {
      gmail.users.messages.list(options, function(err, response) {
        var messages = (response || {}).messages || []
        callback(String(err || ''))(messages)();
      });
    };
  };
};
