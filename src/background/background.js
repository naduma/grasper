import browser from 'webextension-polyfill';


let pageTabId = null;


browser.browserAction.onClicked.addListener(async () => {
  // urlパラメータのscheme部を*にするとextensionがマッチしなくなる為に無指定
  const tabs = await browser.tabs.query({});

  const selfUrl = `://${browser.i18n.getMessage("@@extension_id")}/app/app.html`;
  const targets = tabs.filter(tab => tab.url.endsWith(selfUrl));
  if (targets.length > 0) {
    const first = targets.shift();
    for (const target of targets) {
      await browser.tabs.remove(target.id);
    }

    const currentWindow = await browser.windows.getCurrent({
      windowTypes: [browser.windows.WindowType.NORMAL]
    });
    await browser.tabs.move(first.id, {
      windowId: currentWindow.id,
      index: 0
    });
    await browser.tabs.update(first.id, { active: true, pinned: true });
  }
  else {
    const tab = await browser.tabs.create({
      url: 'app/app.html',
      index: 0,
      pinned: true
    });
    pageTabId = tab.id;
  }
});


const isComplete = async (tabId) => {
  const tab = await browser.tabs.get(tabId);
  if (!tab) return false;
  if (tab.status !== browser.tabs.TabStatus.COMPLETE) return false;
  return true;
};


const TabEvents = [
  'onCreated', 'onRemoved', 'onAttached', 'onDetached', 'onUpdated', 'onMoved', 'onReplaced',
];
for (const te of TabEvents) {
  browser.tabs[te].addListener(async () => {
    if (!pageTabId) return;
    const isOk = await isComplete(pageTabId);
    if (!isOk) return;

    browser.tabs.sendMessage(pageTabId, `Tab: ${te}`);
  });
}


const WindowEvents = [
  'onCreated', 'onRemoved'
];
for (const we of WindowEvents) {
  browser.windows[we].addListener(async () => {
    if (!pageTabId) return;
    const isOk = await isComplete(pageTabId);
    if (!isOk) return;

    browser.tabs.sendMessage(pageTabId, `Window: ${we}`);
  });
}
