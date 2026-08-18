module Jasskell.Icon (bell, acorn, heart, leaf) where

import Data.ByteString (ByteString)
import Lucid
import Lucid.Base

svgIcon :: (Monad m) => ByteString -> [Attributes] -> HtmlT m ()
svgIcon paths as =
  svg_
    (width_ "20" : height_ "20" : class_ "icon" : makeAttributes "viewBox" "0 0 256 256" : as)
    $ toHtmlRaw paths

bell :: (Monad m) => [Attributes] -> HtmlT m ()
bell =
  svgIcon
    """
    <path d="M96,192a32,32,0,0,0,64,0" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
    <path d="M56,104a72,72,0,0,1,144,0c0,35.82,8.3,64.6,14.9,76A8,8,0,0,1,208,192H48a8,8,0,0,1-6.88-12C47.71,168.6,56,139.81,56,104Z" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
    """

acorn :: (Monad m) => [Attributes] -> HtmlT m ()
acorn =
  svgIcon
    """
    <path d="M216,112v16c0,53-88,88-88,112,0-24-88-59-88-112V112" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
    <path d="M80,56h96a48,48,0,0,1,48,48v0a8,8,0,0,1-8,8H40a8,8,0,0,1-8-8v0A48,48,0,0,1,80,56Z" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
    <path d="M128,56V48a32,32,0,0,1,32-32" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
    """

heart :: (Monad m) => [Attributes] -> HtmlT m ()
heart =
  svgIcon
    """
    <path d="M128,224l89.36-90.64a50,50,0,1,0-70.72-70.72L128,80,109.36,62.64a50,50,0,0,0-70.72,70.72Z" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
    """

leaf :: (Monad m) => [Attributes] -> HtmlT m ()
leaf =
  svgIcon
    """
    <path d="M63.81,192.19c-47.89-79.81,16-159.62,151.64-151.64C223.43,176.23,143.62,240.08,63.81,192.19Z" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
    <line x1="160" y1="96" x2="40" y2="216" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="16"/>
    """
