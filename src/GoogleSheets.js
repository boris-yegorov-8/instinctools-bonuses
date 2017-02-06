var sheets = require('googleapis').sheets('v4');

exports.getValues = function(options) {
    return function(success, error) {
        sheets.spreadsheets.values.get(options, function(err, response) {
            if (err) {
                error(err);
            } else {
                console.log('-------------');
                console.log(response);
                console.log('-------------');
                success('42');
            }
        });
    };
};
