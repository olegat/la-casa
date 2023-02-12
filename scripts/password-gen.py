#!/usr/bin/env python3

import argparse
import collections.abc
import enum
import os
import random
import sys

from table_parser import TableException, TableParser, TableParserContext

class Pos(enum.Flag):
  START = enum.auto()
  MID   = enum.auto()
  END   = enum.auto()

  def parse_tablerow(row:dict):
    result = Pos(0)
    for p in row['pos'].split('|'):
      result = result | getattr(Pos, p.upper())
    return result


class Kind(enum.Enum):
  VOWEL     = enum.auto()
  CONSONANT = enum.auto()

  def parse_tablerow(row:dict):
    return getattr(Kind, row['kind'].upper())


class Sym:
  def __init__(self, text:str, kind:Kind, position:Pos):
    self.text     = text
    self.kind     = kind
    self.position = position

  def __repr__(self):
    return f'{{"{self.text}", {self.kind}, {self.position}}}'


class SymList:
  def __init__(self, syms):
    self._iter = syms;

  def _maybe_expand(self):
    if isinstance(self._iter, filter):
      self._iter = list(self._iter)

  def __repr__(self):
    return str(self._iter)

  def __len__(self):
    self._maybe_expand()
    return len(self._iter)

  def __getitem__(self, key):
    self._maybe_expand()
    return self._iter.__getitem__(key)

  def __setitem__(self, key, value):
    self._maybe_expand()
    return self._iter.__setitem__(key, value)

  def filter(self, kind=None, position=None):

    def maybe_in( value, maybe_iter ):
      if isinstance( maybe_iter, collections.abc.Iterable ):
        return value in maybe_iter;
      return False

    def fn(sym):
      return \
        (kind == None or sym.kind == kind or maybe_in(sym.kind, kind)) and \
        (position == None or (sym.position & position) == position)

    return SymList(filter( fn, self._iter ))

  def randelem(self):
    i = random.randint(0, len(self)-1)
    return self._iter[i]


class WordGenerator:
  def __init__(self, symlist:SymList=None, minm=1, maxm=2):
    if symlist == None:
      symlist = SymList()
    self.symlist = symlist
    self.minm = minm
    self.maxm = maxm
    # Precompute filtered lists:
    self._mids = [None, None]
    self._ends = [None, None]
    self._starts = symlist.filter(position=Pos.START)
    self._mids[0] = symlist.filter(kind=Kind.VOWEL,     position=Pos.MID)
    self._mids[1] = symlist.filter(kind=Kind.CONSONANT, position=Pos.MID)
    self._ends[0] = symlist.filter(kind=Kind.VOWEL,     position=Pos.END)
    self._ends[1] = symlist.filter(kind=Kind.CONSONANT, position=Pos.END)

  def gen_symbols_sequence(self):
    result = [self._starts.randelem()]
    i = 0 if result[0].kind == Kind.CONSONANT else 1
    for _ in range( random.randint(self.minm, self.maxm) ):
      result.append( self._mids[i].randelem() )
      i = (i + 1) % 2
    result.append(self._ends[i].randelem())
    return result


# Many backend require at least one number and one capital letter.
# This special generator generates exactly of each.
class TagGenerator:
  def __init__(self):

    def _syms(texts:list, kind:Kind):
      return [Sym(str(text), kind, Pos.START|Pos.END) for text in texts]

    # Exclude [1, I] and [0, O] which are easily confused.
    nums  = _syms([2, 3, 4, 5, 6, 7, 8, 9], Kind.VOWEL)
    chars = _syms(list('ABCDEFGHJKLMNPQRSTUVWXYZ'), Kind.CONSONANT)
    self._wg = WordGenerator(SymList(nums+chars), minm=0, maxm=0)

  def gen_symbols_sequence(self):
    return self._wg.gen_symbols_sequence();


class PasswordGenerator:
  def __init__(self, symlist:SymList=None):
    self._wg = WordGenerator(symlist)
    self._tg = TagGenerator();

  def gen_password(self):
    gs = [self._wg] * 3
    gs.insert( random.randint(0,2), self._tg )
    words = [''.join([s.text for s in g.gen_symbols_sequence()]) for g in gs]
    return '-'.join(words)


class SymbolParser(TableParser):
  def validate_row(self, row:dict, context:TableParserContext):
    success = True
    kind = row['kind']
    pos  = row['pos']
    if not kind in ['consonant', 'vowel']:
      context.log(f"Unknown kind '{kind}' (expected 'consonant' or 'vowel')")
      success = False
    for p in pos.split('|'):
      if not p in ['start', 'mid', 'end']:
        context.log(f"Unknown position '{p}' (expected 'start', 'mid' or 'end')")
        success = False
    return success


def parse_symbolist(path:str):
  tp = SymbolParser( 3, ['text', 'kind', 'pos'] )
  result = []
  for row in tp.parse_file(path):
    kind = Kind.parse_tablerow(row)
    pos = Pos.parse_tablerow(row)
    symbol = Sym( row['text'], kind, pos )
    result.append( symbol )
  return SymList(result)

def rebase_path(filename:str):
  return os.path.join(os.path.dirname(__file__), filename)

def parse_argv(argv):
  parser = argparse.ArgumentParser(
    description="generate passwords using made up words "
    "(that look like real words)")
  parser.add_argument(
    '-f', '--symbolist-file', nargs=1,
    default=rebase_path('symbols-englishy.txt'))
  return parser.parse_args(argv)

def main(argv):
  args = parse_argv(argv[1:])
  print(args)
  pwg = PasswordGenerator( parse_symbolist(args.symbolist_file) )
  passwords = [pwg.gen_password() for _ in range(10)]
  print("Here are some suggested passwords:")
  print('\n  '.join(['']+passwords))
  # wg = WordGenerator()
  # for _ in range(1000):
  #   slist = [x.text for x in wg.gen_symbols_sequence()]
  #   print( f'{"".join(slist)}  ({", ".join(slist)})')
  return 0

if __name__ == '__main__':
  exit(main(sys.argv))
