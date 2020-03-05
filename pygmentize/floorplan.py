from pygments.lexer import RegexLexer
from pygments.token import *
import re

__all__ = ['FloorplanLexer']

class FloorplanLexer(RegexLexer):
  name = 'Floorplan'
  aliases = ['floorplan']
  filenames = ['*.flp']

  flags = re.MULTILINE | re.UNICODE

  reserved = ( 'ptr', 'flags', '_', 'Counter', 'bit'
             , 'bits', 'byte', 'bytes', 'sizeof', 'List'
             , 'word', 'words', 'page', 'pages'
             , 'enum', 'union', 'seq', 'choice')

  tokens = {
      'root': [
          # Whitespace:
          (r'\s+', Text),
          # Comments:
          (r'\/\/.*$', Comment.Single),
          (r'/\*', Comment.Multiline, 'comment'),

          # Identifiers:
          (r'(%s)\b' % '|'.join(reserved), Keyword.Reserved),
          (r'[A-Z]\w*',  Keyword.Type),
          (r'[a-z_]\w*', Name),

          # Operators:
          (r'({|}|#|@|\^|=>|\.\.\.|\.|->|:|,|=\||~\||\|)', Operator.Word),
          (r'[-+\*\/\[\]\<\>]',       Operator),

          # Numeric Literals:
          (r'0[xX][\da-fA-F]+', Number.Hex), # hex literals
          (r'0[bB][01]+', Number.Bin), # binary literals
          (r'\d+', Number.Integer), # decimal literals

          # Special:
          (r'[\(\)]', Operator), # parentheses
			],
      'comment': [
					# Multiline Comments
					(r'[^\*/]+', Comment.Multiline),
					(r'/\*', Comment.Multiline, '#push'),
					(r'\*/', Comment.Multiline, '#pop'),
					(r'[\*/]', Comment.Multiline),
			],
  }

