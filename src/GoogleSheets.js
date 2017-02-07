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
    // TODO: move request to purs
    var request = {
      auth: options.auth,
      spreadsheetId: options.spreadsheetId,
      resource: {
        requests: [
          // {
          //   updateCells: {
          //     start: { sheetId: 0, rowIndex: 156, columnIndex: 0 },
          //     rows: [
          //       {
          //         values: [
          //           {
          //             userEnteredValue: { numberValue: 1 },
          //           }
          //         ]
          //       }
          //     ],
          //     fields: 'userEnteredValue'
          //   }
          // },
          // {
          //   copyPaste: {
          //     source: {
          //       sheetId: 0,
          //       startRowIndex: 152,
          //       endRowIndex: 153,
          //       startColumnIndex: 7,
          //       endColumnIndex: 8
          //     },
          //     destination: {
          //       sheetId: 0,
          //       startRowIndex: 153,
          //       endRowIndex: 154,
          //       startColumnIndex: 7,
          //       endColumnIndex: 8
          //     },
          //     pasteType: 'PASTE_FORMULA'
          //   }
          // },
          {
            insertDimension: {
              range: {
                sheetId: 0,
                dimension: 'ROWS',
                startIndex: 154,
                endIndex: 160,
              },
              inheritFromBefore: true,
            }
          }
        ]
      }
    };

    sheets.spreadsheets.batchUpdate(request, function(err, response) {
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
