class TableException(Exception):
  pass


class TableParserContext:
  def __init__(self, path:str):
    self.path   = path
    self.line   = None
    self.linum  = 1
    self.errlog = []

  def log(self, message:str):
    if isinstance(self.errlog, list):
      self.errlog.append(
        f'error: {self.path}:{self.linum}: {message}:\n'
        f'    | {self.line}\n'+
        f'    | ^')


class TableParser:
  def __init__(self, ncols, colnames=None):
    if colnames == None:
      colnames = list(range(ncols))
    self.ncols = ncols
    self.colnames = colnames

  def validate_row(self, row:dict, context:TableParserContext):
    return True

  def _parse_line(self, context:TableParserContext):
    # filter empty lines, spaces and comments
    (result, success) = (None, True)
    line = context.line.split('#')[0].strip()
    if line:
      fields = list(filter(lambda x: x, context.line.split(' ')))
      if len(fields) != self.ncols:
        success = False
        context.log(f'Requires {self.ncols} columns (found {len(fields)})')
      else:
        result = {self.colnames[i] : fields[i] for i in range(self.ncols)}
        if not self.validate_row(result, context):
          (result, success) = (None, False)
    return result, success

  def parse_file(self, path):

    def read_lines(path):
      with open(path, 'r') as f:
        return f.read().strip().split('\n')

    (result, err, context) = ([], False, TableParserContext(path))
    for context.line in read_lines(path):
      (entry, success) = self._parse_line(context)
      err = err or not success
      if entry:
        result.append(entry)
      context.linum += 1

    if err:
      print('\n\n'.join(context.errlog))
      raise TableException()

    return result
