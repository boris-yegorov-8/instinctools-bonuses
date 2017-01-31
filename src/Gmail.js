var gmail = require('googleapis').gmail('v1');

exports.getMessages = function(options) {
  return function(success, error) {
    gmail.users.messages.list(options, function(err, response) {
      if (err) {
        error(err);
      } else {
        success((response || {}).messages || []);
      }
    });
  };
};

exports.getMessage = function (options) {
  return function(success, error) {
    gmail.users.messages.get(options, function(err, response) {
      if (err) {
        error(err);
      } else {
        console.log('-----------');
        console.log(response.payload.parts[0].body.data);
        console.log('-----------');
        success(response);
      }
    });
  };
};
