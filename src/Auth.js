var googleAuth = require('google-auth-library');
var auth = new googleAuth();
var oauth2Client = null;

exports.createClient = function(options) {
  oauth2Client = new auth.OAuth2(
    options.clientId,
    options.clientSecret,
    options.redirectUri
  );

  return oauth2Client;
};

exports.setToken = function (token) {
  oauth2Client.credentials = token;

  return oauth2Client;
};

exports.generateAuthUrl = function (oauth2Client) {
  return function (options) {
    return oauth2Client.generateAuthUrl(options);
  };
};

exports.getToken = function (oauth2Client) {
  return function (code) {
    return function(callback) {
      return function() {
        oauth2Client.getToken(code, function(err, token) {
          callback(String(err || ''))(token)();
        });
      };
    };
  }
};
