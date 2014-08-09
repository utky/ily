module Ily.Compiler.Syntax () where

data Document = [Element]

data Element = Block | Inline

data Block = Header | Paragraph 

data Paragraph = [Inline]

data Header = Chaper | Section | Scene

data List = BulletList -- | OrderedList | DefinitionList

data BulletMark = '*' | '+' | '-'

data Inline = Desc | Comment | Annotation | Commands

data Comment = (CommentMark, Desc)

data CommentMark = '%'

data Annotation = (AnnotationMark, Desc)

data AnnotationMark = '@'

data Commands

data Desc = ByteString

data LineBreak = '\n'

data Space = [SpaceChar]

data SpaceChar = ' ' | '　'

