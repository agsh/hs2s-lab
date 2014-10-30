import urllib2
from BeautifulSoup import BeautifulSoup


Paradigms = {
    0: 'Imperative',
    1: 'Object-oriented',
    2: 'Functional',
    3: 'Procedural',
    4: 'Generic',
    5: 'Reflective',
    6: 'Event-driven'
}


class Language:

    def __repr__(self):
        return str([self.title, self.usage, self.styles, self.standard])

    def __init__(self, title, usage, styles, standard):
        self.title = title
        self.usage = usage
        self.styles = styles
        self.standard = standard


def parse_language_from_row(row):
    col1 = row.find('th')
    title = col1.text
    properties = row.findAll('td')
    usage = properties[0].text.split(',')
    styles = []
    i = 0
    for style in properties[1:-2]:
        if 'Yes' in style:
            styles.append(Paradigms[i])
        i += 1
    standard = properties[len(properties) - 1].text
    if standard[-1] == ']':
        standard = standard[:-3]

    return Language(title, usage, styles, standard)


page = urllib2.urlopen('http://en.wikipedia.org/wiki/Comparison_of_programming_languages')
soup = BeautifulSoup(page)

table = soup.findAll('table', {'class': 'wikitable sortable'})[0]
rows = table.findAll('tr')[1:-1]

langs = []

for row in rows:
    if row.findAll('td'):
        langs.append(parse_language_from_row(row))

imperative_but_not_functional_langs = [lang for lang in langs if 'Imperative' in lang.styles and not 'Functional' in lang.styles]
print "\n".join([x.title for x in imperative_but_not_functional_langs])