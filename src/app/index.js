import browser from 'webextension-polyfill';
import './assets/css/style.css';
import { Elm } from './elm/Main.elm';


// app init
const app = Elm.Main.init({
  node: document.getElementById('app')
});


// ports
app.ports.getTabs.subscribe(async () => {
  const rawTabs = await browser.tabs.query({});
  const tabs = rawTabs.map((tab) => {
    return {
      id: tab.id,
      title: tab.title,
      url: tab.url,
      favicon: tab.favIconUrl === undefined ? '' : tab.favIconUrl,
      windowId: tab.windowId,
    };
  });
  app.ports.gotTabs.send(JSON.stringify(tabs));
});


browser.runtime.onMessage.addListener(message => {
  app.ports.updatedBrowser.send(message);
});


app.ports.openTab.subscribe(async (tabId) => {
  const tabs = await browser.tabs.query({});
  const tab = tabs.find((tab) => tab.id === tabId);
  if (!tab) return;
  await browser.tabs.update(tabId, { active: true });
  await browser.windows.update(tab.windowId, { focused: true });
});


app.ports.deleteTabs.subscribe((tabIds) => {
  browser.tabs.remove(tabIds);
});


app.ports.moveTabs.subscribe(async (args) => {
  const { tabIds, windowId } = args;

  if (windowId === null) {
    const firstId = tabIds.shift();
    try {
      const newWindow = await browser.windows.create({
        tabId: firstId,
        state: 'minimized',
        focused: false
      });
      if (tabIds.length > 0) {
        await browser.tabs.move(tabIds, {
          windowId: newWindow.id,
          index: -1
        });
      }
      await browser.windows.update(newWindow.id, {
        state: 'normal',
        focused: false
      });
    }
    catch (ex) {
      /*
        https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/API/windows/create
        firefoxはcreate時のfocusedパラメータ未対応の為
        minimizedでも画面のチラツキが発生するので最小サイズで対応
      */
      const focusedWindow = await browser.windows.getLastFocused({
        populate: true,
        windowTypes: ['normal'],
      });
      const newWindow = await browser.windows.create({
        tabId: firstId,
        state: 'normal',
        height: 1, width: 1, top: 1, left: 1,
      });
      await browser.windows.update(focusedWindow.id, { focused: true });
      if (tabIds.length > 0) {
        await browser.tabs.move(tabIds, {
          windowId: newWindow.id,
          index: -1
        });
      }
      await browser.windows.update(newWindow.id, {
        focused: false,
        height: focusedWindow.height,
        width: focusedWindow.width,
        top: focusedWindow.top,
        left: focusedWindow.left,
      });
      await browser.windows.update(focusedWindow.id, { focused: true });
    }
  }
  else {
    await browser.tabs.move(tabIds, {
      windowId: windowId,
      index: -1
    });
  }
});
