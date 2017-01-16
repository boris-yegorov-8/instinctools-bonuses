var googleAuth = require('google-auth-library');
var auth = new googleAuth();
var oauth2Client = null;

exports.createClient = function(options) {
  oauth2Client = new auth.OAuth2(
    options.clientId,
    options.clientSecret,
    options.redirectUrl
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
