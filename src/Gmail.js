var gmail = require('googleapis').gmail('v1');

exports.getMessages = function(options) {
  return function(success, error) {
    gmail.users.messages.list(options, function(err, response) {
      if (err) {
        error(err);
      } else {
        success(response || {}).messages || []);
      }
    });
  };
};
