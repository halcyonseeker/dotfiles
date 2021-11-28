
# Load notification preferences and stuff from config.yml
config.load_autoconfig(True)

# Restore previous sessions
c.auto_save.session = True

# Preview PDFs in pdf.js
c.content.pdfjs = True

# Use Searx as the default search engine and new-tab page
# c.url.searchengines = { "DEFAULT" : "https://searx.com/search?q={}" }
c.url.default_page = "~/.config/qutebrowser/start.html"
c.url.start_pages = ["~/.config/qutebrowser/start.html"]

# Emacs-like keybindings
#config.unbind('<ctrl-x>', mode='normal')
c.bindings.commands['normal'] = {
    '<alt-x>': 'set-cmd-text :',
    #'<ctrl-x><ctrl-c>': 'quit',
    #'<ctrl-x><b>': 'tab-select',
    #'<ctrl-x><k>': 'tab-close',
}
c.bindings.commands['command'] = {
    '<ctrl-p>': 'completion-item-focus prev',
    '<ctrl-n>': 'completion-item-focus next',
}

# Spoof my user agent
# c.content.headers.user_agent = "Mozilla/5.0 (Windows NT 10.0; rv:68.0) Gecko/20100101 Firefox/89.0"
