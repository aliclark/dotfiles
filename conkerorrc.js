
active_hint_background_color = '#00FF00';
active_img_hint_background_color = '#00FF00';
clicks_in_new_buffer_button = 1;
require("clicks-in-new-buffer.js");

homepage = 'http://www.google.co.uk/';

define_webjump("reddit", "http://www.reddit.com/");
define_webjump("proggit", "http://www.reddit.com/r/programming");
define_webjump("subbit", "http://www.reddit.com/r/%s");

define_webjump("hn", "http://news.ycombinator.com/");

define_webjump("gmail", "https://mail.google.com/");

define_webjump("antwiki", function (s) {
    if (s) {
        return "http://wiki/index.php?title=Special%3ASearch&search=" + encodeURIComponent(s) + "&go=Go";
    } else {
        return "http://wiki/";
   }
}, $argument = "optional");



define_webjump("antmail", "http://cam-mail2k7/owa/");

define_webjump("skyjira", "http://jira/secure/IssueNavigator.jspa?reset=true&jqlQuery=project+%3D+CON+AND+resolution+%3D+Unresolved+ORDER+BY+due+ASC%2C+priority+DESC%2C+created+ASC&mode=hide");




define_webjump("bookmark",
               function(term) {return term;},
               $completer = history_completer($use_history = false,
                                              $use_bookmarks = true,
                                              $match_required = true),
               $description = "Visit a conkeror bookmark");

define_webjump("emacswiki",
    "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi"+
        "&q=%s&sa=Search&siteurl=emacswiki.org%2F",
    $alternative="http://www.emacswiki.org/");

// magick-options is a webjump for imagemagick command line options.
//
// magick-options caches its completions in a preference.  To clear the cache
// and force magick-options to fetch the information anew, just do:
//
//   clear_pref('conkeror.webjump.magick-options.cache');
//
// last modified: November 25, 2009
//
function magick_options_completer (input, cursor_position, conservative) {
    var completions;
    try {
        completions = get_pref('conkeror.webjump.magick-options.cache').split(' ');
    } catch (e) { }
    if (! completions) {
        try {
            var content = yield send_http_request(
                load_spec({uri: "http://www.imagemagick.org/script/command-line-options.php"}));
            completions = content.responseText
                .match(/([a-z]+)(?=\">-\1<\/a>)/g)
                .filter(remove_duplicates_filter());
            user_pref('conkeror.webjump.magick-options.cache', completions.join(' '));
        } catch (e) {
            completions = [];
        }
    }
    yield co_return(prefix_completer($completions = completions)
                    (input, cursor_position, conservative));
}
define_webjump("magick-options",
    "http://www.imagemagick.org/script/command-line-options.php#%s",
    $alternative = "http://www.imagemagick.org/script/command-line-options.php",
    $completer = magick_options_completer);

define_webjump("imdb", "http://imdb.com/find?q=%s");

define_webjump("archive.org", "http://www.archive.org/search.php?query=%s");

define_webjump("youtube", "http://www.youtube.com/results?search_query=%s&search=Search");

define_webjump("finance", "http://www.google.com/finance?q=%s");

define_webjump("anagram", "http://wordsmith.org/anagram/anagram.cgi?anagram=%s&t=1000&a=n");

define_xpath_webjump(
    "conkerorwiki-page",
    "http://conkeror.org/",
    '//xhtml:li/xhtml:p/xhtml:a[starts-with(@href,"/")]',
    $description = "Conkeror wiki pages linked from the front page");

define_webjump("trans", "http://translate.google.com/translate_t#auto|en|%s");

define_webjump("urban", "http://www.urbandictionary.com/define.php?term=%s");

define_webjump("down?", function (url) {
    if (url) {
        return "http://downforeveryoneorjustme.com/" + url;
    } else {
        return "javascript:window.location.href='http://downforeveryoneorjustme.com/'+window.location.href;";
   }
}, $argument = "optional");

define_webjump("wayback", function (url) {
    if (url) {
        return "http://web.archive.org/web/*/" + url;
    } else {
        return "javascript:window.location.href='http://web.archive.org/web/*/'+window.location.href;";
    }
}, $argument = "optional");

define_webjump("news", "http://news.google.com/news/search?q=%s");

define_webjump("hackernews", "http://www.google.com/search?q=site:news.ycombinator.com+%s",
               $alternative = "http://news.ycombinator.com/");

define_webjump("searchyc", "http://searchyc.com/%s",
               $alternative = "http://news.ycombinator.com/");

define_webjump("bashfaq",
    "http://mywiki.wooledge.org/BashFAQ?action=fullsearch&context=180&value=%s&fullsearch=Text",
    $alternative = "http://mywiki.wooledge.org/BashFAQ");

define_webjump("commandlinefu",
    function(term) {
        return 'http://www.commandlinefu.com/commands/matching/' +
            term.replace(/[^a-zA-Z0-9_\-]/g, '')
                .replace(/[\s\-]+/g, '-') +
            '/' + btoa(term);
    },
    $argument = 'optional',
    $alternative = "http://www.commandlinefu.com/");

define_webjump("codesearch", "http://www.google.com/codesearch?q=%s");

define_webjump("mdc", "https://developer.mozilla.org/Special:Search?search=%s&type=fulltext&go=Search");

define_webjump("hoogle", "http://haskell.org/hoogle/?hoogle=%s",
               $alternative = "http://haskell.org/hoogle/");

require("page-modes/wikipedia.js");
wikipedia_webjumps_format = "wp-%s"; // controls the names of the webjumps.  default is "wikipedia-%s".
define_wikipedia_webjumps("en"); // For English
// define_wikipedia_webjumps(); // To make use of ALL of the webjumps (200+).

define_webjump("alpha", "http://www36.wolframalpha.com/input/?i=%s");


define_webjump("blekko", "http://blekko.com/ws/%s", $alternative = "http://blekko.com/");
define_webjump("duckduckgo", "http://duckduckgo.com/?q=%s");

define_webjump("imagesgoogle", "http://www.google.com/images?q=%s", $alternative = "http://www.google.com/imghp");

define_webjump("deb", "http://packages.debian.org/search?keywords=%s&searchon=names&suite=testing&section=all");
define_webjump("debfile", "http://packages.debian.org/search?searchon=contents&keywords=%s&mode=path&suite=testing&arch=any");
define_webjump("debbugs", "http://bugs.debian.org/%s");
define_webjump("debpts", "http://packages.qa.debian.org/%s");

define_webjump("github", "http://github.com/search?q=%s&type=Everything");
define_webjump("gitorious", "http://gitorious.org/search?q=%s");

define_webjump("weather", "http://www.wunderground.com/cgi-bin/findweather/getForecast?query=%s");



define_mime_type_external_handler("application/msword", "openoffice.org");


//user_pref('extensions.mozrepl.autoStart', true);

// Mozrepl
//
if ('@hyperstruct.net/mozlab/mozrepl;1' in Cc) {
  let mozrepl = Cc['@hyperstruct.net/mozlab/mozrepl;1']
    .getService(Ci.nsIMozRepl);
  if (! mozrepl.isActive())
    mozrepl.start(4242);
}

let (mozrepl_init = get_home_directory()) {
    mozrepl_init.appendRelativePath("~/.conkeror-mozrepl.js");
    session_pref('extensions.mozrepl.initUrl', make_uri(mozrepl_init).spec);
}

