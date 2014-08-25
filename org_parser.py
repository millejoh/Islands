import re, io


HEADING = re.compile(r'([*]+)\s+(.*)')


class Node(object):
    def __init__(self, name, parent):
        self.name = name
        self.parent = parent
        self.children = []

    def add_child(self, obj):
        print("! {0}: Adding child {1}".format(self, obj))
        self.children.append(obj)

    def remove_child(self, obj):
        self.children.remove(obj)

    def __repr__(self):
        return "Node({0},{1})".format(self.name, self.parent)


class NodeContent(object):
    def __init__(self, data, parent):
        self._data = data
        self.parent = parent


class OrgParser(object):
    def __init__(self, org_file_iter):
        self.org_file = org_file_iter
        self.header = Node(None, None)
        self.header_level = 1
        self.content = []
        self.props = []
        self.root = []
        self.current_line = None

    def __iter__(self):
        self.current_line = next(self.org_file)
        while True:
            if HEADING.match(self.current_line):
                yield self.process_heading()
                self.current_line = next(self.org_file)
            else:
                self.content = [cline for cline in self.process_content()]
                yield NodeContent(self.content, self.header)
                self.content = []

    def process_content(self):
        print("! Process content: {0}".format(self.current_line))
        while True:
            yield self.current_line
            self.current_line = next(self.org_file)
            print("! Process content: match={0}".format(HEADING.match(self.current_line)))

    def skip_levels(self, levels):
        for l in range(levels - 1):
            obj = Node(None, self.header)
            self.header = obj

    def process_heading(self):
        mat = HEADING.match(self.current_line)
        level, name = len(mat.group(1)), mat.group(2)
        #print('New Level = {0}, Previous Level = {1}'.format(level, self.header_level))
        if level == 1:
            obj = Node(name, None)
            self.header = obj
            self.root.append(obj)
            return obj
        elif level == self.header_level:
            obj = Node(name, self.header.parent)
            self.header = obj
            return obj
        elif level > self.header_level:
            if level - self.header_level > 1:
                self.skip_levels(level - self.header_level)
            self.header_level = level
            obj = Node(name, self.header)
            self.header = obj
            return obj
        elif level < self.header_level:
            self.header_level = level
            obj = Node(name, self.header.parent.parent)
            self.header = obj
            return obj


simple_header_data = u"""* Heading 1
** Heading 2.a
*** Heading 3.a
*** Heading 3.b
** Heading 2.b
*
"""

complex_header_data = u"""* Heading 1.a
* Heading 1.b
** Heading 2
**** Heading 4.a
*** Heading 3
* Heading 1.c
"""

header_content_data = u"""* Heading 1
Some text in this heading.
** Heading 2
   Some more text, indented.
*** Heading
Here is
some multine
text.

 - And some markdown stuff for kicks and giggles.
"""


def test_heading_re():
    h1 = '* Heading 1'
    h2 = '** Heading 2'
    h3 = '*** Heading 3'
    h9 = '********* Heading 9'
    not_heading = 'This is some text that is not a heading.'
    assert HEADING.match(h1) is not None
    assert HEADING.match(h2) is not None
    assert HEADING.match(h3) is not None
    assert HEADING.match(h9) is not None
    assert HEADING.match(not_heading) is None


def test_heading_parser():
    stream = io.StringIO(simple_header_data)
    reader = OrgParser(stream)
    print('Simple Header Data:')
    for obj in reader:
        if obj:
            print(obj)
        #			print(" Children:{0}".format(obj.children))
    print('\nComplex Header Data:')
    stream = io.StringIO(complex_header_data)
    reader = OrgParser(stream)
    for obj in reader:
        if obj:
            print(obj)


#			print(" Children:{0}".format(obj.children))


def test_content():
    stream = io.StringIO(header_content_data)
    reader = OrgParser(stream)
    for obj in reader:
        if obj:
            print(obj)
