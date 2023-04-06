#!/usr/bin/env python3

import argparse
import collections.abc
import enum
import math
import os
import random
import sys

from table_parser import TableException, TableParser, TableParserContext
import json

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

  def inverse(self):
    if self == Kind.VOWEL:
      return Kind.CONSONANT
    else:
      return Kind.VOWEL


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
  def __init__(self, symlist:SymList, minm, maxm):
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

  def _count_map(self):
    counts = {}
    for position in [Pos.START, Pos.MID, Pos.END]:
      counts[position] = {}
      for kind in [Kind.CONSONANT, Kind.VOWEL]:
        ls = self.symlist.filter(kind=kind, position=position)
        counts[position][kind] = len(ls)
    return counts

  def _permutation_count(self, start_kind):
    # TODO: check for "duplicate" permutations
    # TODO: break down the math into a permutation_stats dict.
    (total, counts) = (0, self._count_map())
    # Zero times something is zero. No point looping if there's no starting
    # symbols of kind `start_kind`:
    if counts[Pos.START][start_kind] == 0:
      return 0
    for midcount in range(self.minm, self.maxm+1):
      # The number of Pos.START symbols
      (subtotal, kind) = (counts[Pos.START][start_kind], start_kind.inverse())
      # The number of permutations of Pos.MID symbols
      for _ in range(midcount):
        subtotal = subtotal * counts[Pos.MID][kind]
        kind = kind.inverse()
      # The number of Pos.END symbols
      total += (subtotal * counts[Pos.END][kind])
    return total

  def _permutation_subset(self, start_kind):
    return { 'expr' : str(self._permutation_count(start_kind)) }

  def permutation_stats(self):
    return {
      'expr' : 'WORDS_V + WORDS_C',
      'help' : 'The total number of permutations is the sum of: all the '
               'permutations starting with "vowel"-kind symbols, and all the '
               'the permutations starting with "consonant"-kind symbols.',
      'vars' :
      {
        'WORDS_V' : self._permutation_subset(Kind.VOWEL),
        'WORDS_C' : self._permutation_subset(Kind.CONSONANT),
      },
    }


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

  def permutation_stats(self):
    return {
      'expr' : '2 * TAG_PAIRS',
      'help' : 'Each letter/number pair has two permutations: it either start '
               'with the number, or starts the letter',
      'vars' :
      {
        'TAG_PAIRS':
        {
          'expr' : 'TAG_NUMS * TAG_ALPHAS',
          'help' : "The number of possible letter/numbers pair (ordering "
                   "ignored) is equal to the product of each set's size.",
          'vars' :
          {
            'TAG_NUMS':
            {
              'expr' : '8', # FIXME: hardcoded TAG_NUMS
              'help' : 'There are 8 possible numbers (0 and 1 are not used '
                       'because they can easily be misinterpretted as letters)'
            },
            'TAG_ALPHAS':
            {
              'expr' : '24', # FIXME: hardcoded TAG_ALPHAS
              'help' : 'There are 24 possible letter (O and I are not used '
                       'because they can easily be misinterpretted as numbers)'
            },
          }
        }
      }
    }


class PasswordGenerator:
  def __init__(self, wordgen:WordGenerator):
    self._wg = wordgen
    self._tg = TagGenerator()

  def gen_password(self):
    gs = [self._wg] * 3
    gs.insert( random.randint(0,3), self._tg )
    words = [''.join([s.text for s in g.gen_symbols_sequence()]) for g in gs]
    return '-'.join(words)

  def permutation_stats(self):
    return {
      'expr' : 'ALL_WORDS * ALL_TAGS',
      'help' : 'The total permutations is the products of all the word '
               'permutations and all the tag permutations.',
      'vars' :
      {
        'ALL_TAGS' :
        {
          'expr' : '4 * TAGS', # FIXME: hardcoded TAGS_PLACES=4.
          'help' : 'Each tag permutation can appear in 4 different places.',
          'vars' : { 'TAGS' : self._tg.permutation_stats() }
        },
        'ALL_WORDS' :
        {
          'expr' : 'WORDS ^ 3', # FIXME: hardcoded WORD_COUNT=3.
          'help' : 'There are three words.',
          'vars' : { 'WORDS' : self._wg.permutation_stats() },
        },
      },
    }


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


class ExpressionValue:
  def __init__(self, value):
    self.value = int(value)

  def expand(self):
    return self.value


class ExpressionVariable:
  def __init__(self, name, expr):
    self.name = name
    self.expr = expr

  def expand(self):
    return self.expr.expand()


class ExpressionOperator:
  def __init__(self, operator, operand1, operand2):
    self.operator = ExpressionOperator.operators()[operator]
    self.operand1 = operand1
    self.operand2 = operand2

  @classmethod
  def operators(cls):
    return { '^': lambda x,y: math.pow(x,y),
             '*': lambda x,y: x * y,
             '+': lambda x,y: x + y }

  def expand(self):
    return self.operator( self.operand1.expand(), self.operand2.expand() )


def parse_permutation_stats_expr(stats:dict):
  # print('')
  # print(f'{stats=}')
  variables = {
    name: parse_permutation_stats_expr(stats['vars'][name])
    for name in stats.get('vars', [])
  }
  # Parse token in postfix notation:
  tokens = stats['expr'].split()
  if len(tokens) == 3:
    (tokens[1], tokens[2]) = (tokens[2], tokens[1])
  # Parse tokens into expressions:
  exprs = []
  for token in tokens:
    if token in variables:
      exprs.append( ExpressionVariable(token, variables[token]) )
    elif token in ExpressionOperator.operators():
      op2 = exprs.pop()
      op1 = exprs.pop()
      exprs.append( ExpressionOperator(token, op1, op2) )
    else:
      exprs.append( ExpressionValue(token) )
  assert len(exprs) == 1
  return exprs[0]

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
    '--midmin', type=int, nargs=1, metavar='X', default=[1],
    help='minimum number of middle symbols')
  parser.add_argument(
    '--midmax', type=int, nargs=1, metavar='Y', default=[2],
    help='maximum number of middle symbols')
  parser.add_argument(
    '-w', '--gen-words', type=int, nargs=1, metavar='N',
    help='generate N words')
  parser.add_argument(
    '-f', '--symbols', nargs=1, metavar='FILE',
    default=[rebase_path('symbols-englishy.txt')],
    help='path to the symbolist text file')
  parser.add_argument(
    '--stats', action='store_true',
    default=False,
    help='print stats report for a symbolist file')
  return parser.parse_args(argv)

def main(argv):
  args = parse_argv(argv[1:])
  wg = WordGenerator(
    symlist = parse_symbolist(args.symbols[0]),
    minm = args.midmin[0],
    maxm = args.midmax[0])
  pwg = PasswordGenerator(wg)
  if args.gen_words:
    for _ in range(args.gen_words[0]):
      slist = [x.text for x in wg.gen_symbols_sequence()]
      print( f'{"".join(slist)}  ({", ".join(slist)})')
  elif args.stats:
    stats = pwg.permutation_stats()
    print(json.dumps(stats, indent=2))
    expr = parse_permutation_stats_expr( stats )
    print(expr)
    print(expr.expand())
  else:
    pwg = PasswordGenerator(wg)
    passwords = [pwg.gen_password() for _ in range(10)]
    print("Here are some suggested passwords:")
    print('\n  '.join(['']+passwords))
  return 0

if __name__ == '__main__':
  exit(main(sys.argv))
