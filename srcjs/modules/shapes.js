import { registerShape, Util } from '@antv/g2';

const registerShapes = () => {

  // https://g2.antv.vision/en/docs/api/advanced/register-shape
  registerShape('interval', 'triangle', {
    // 1. 定义关键点
    getPoints(cfg) {
      const x = cfg.x;
      const y = cfg.y;
      const y0 = cfg.y0;
      const width = cfg.size;
      return [
        { x: x - width / 2, y: y0 },
        { x: x, y: y },
        { x: x + width / 2, y: y0 },
      ];
    },
    // 2. 绘制
    draw(cfg, group) {
      const points = this.parsePoints(cfg.points); // 将0-1空间的坐标转换为画布坐标
      const polygon = group.addShape('path', {
        attrs: {
          path: [
            ['M', points[0].x, points[0].y],
            ['L', points[1].x, points[1].y],
            ['L', points[2].x, points[2].y],
          ],
          ...cfg.defaultStyle,
        },
      });
      return polygon;
    },
  });

}

export { registerShapes }
