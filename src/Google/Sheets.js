var sheets = require('googleapis').sheets('v4');

exports.getValues = function(options) {
  return function(success, error) {
    sheets.spreadsheets.values.get(options, function(err, response) {
      if (err) {
        error(err);
      } else {
        success(response);
      }
    });
  };
};

exports.batchUpdate = function(options) {
  return function(success, error) {
    sheets.spreadsheets.batchUpdate(
      {
        auth: options.auth,
        spreadsheetId: options.spreadsheetId,
        resource: {
          requests: options.resource.requests.map(function (request) { return request.value0; }),
        },
      },
      function(err) {
        if (err) {
          error(err);
        } else {
          success();
        }
      }
    );
  };
};
