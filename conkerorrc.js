
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

define_webjump("antwiki", "http://wiki/");
define_webjump("antmail", "http://cam-mail2k7/owa/");

define_webjump("skyjira", "http://jira/secure/IssueNavigator.jspa?reset=true&jqlQuery=project+%3D+CON+AND+resolution+%3D+Unresolved+ORDER+BY+due+ASC%2C+priority+DESC%2C+created+ASC&mode=hide");


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

