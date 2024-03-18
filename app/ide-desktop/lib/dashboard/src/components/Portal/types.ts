import { MutableRefObject, ReactNode, RefObject } from 'react';

export interface PortalProps {
  /**
   * Ref, where `<Portal />` should render its children
   * By default it renders under `<Root />`
   *
   * @default null
   */
  root?: MutableRefObject<HTMLElement | null> | RefObject<HTMLElement | null>;
  /**
   * Disables portal's API
   *
   * @default false
   */
  isDisabled?: boolean;
  /**
   * Callback, will be called after portal's children mounted
   */
  onMount?: () => void;
  children?: ReactNode;
}
